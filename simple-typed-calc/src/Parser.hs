module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Data.List (elemIndex)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax

lexer = Tok.makeTokenParser style
    where
        ops = ["\\", "+", "*"]
        names = ["if", "then", "else"]
        style = emptyDef {
            Tok.commentLine = "--"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames = names
            }

reservedOp = Tok.reservedOp lexer

reserved = Tok.reserved lexer

parens = Tok.parens lexer

ident = Tok.identifier lexer

type LCParser = Parsec String Context Term

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

--binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc
binary s f assoc = Ex.Infix (opInfo s f) assoc
    where opInfo name op = do
            reservedOp name
            pos <- getPosition
            return $ TmBinOp (infoFrom pos) op

table = [[binary "*" Times Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft]]

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped..."

parseBinExp :: LCParser
parseBinExp = Ex.buildExpressionParser table factor

parseInt :: LCParser
parseInt = do
    pos <- getPosition
    n <- Tok.integer lexer
    return $ TmInt (infoFrom pos) (fromIntegral n)

parseAbs :: LCParser
parseAbs = do
    reservedOp "\\"
    v <- ident
    -- Prepend var onto content, is bound
    modifyState ((v, NameBind) :)
    reservedOp "->"
    term <- parseBinExp
    -- Leaving the scope of the abstraction, pop var off content
    modifyState tail
    pos <- getPosition
    return $ TmAbs (infoFrom pos) v term

parseIf :: LCParser
parseIf = do
    reserved "if"
    cond <- parseBinExp
    reserved "then"
    t1 <- parseBinExp
    reserved "else"
    t2 <- parseBinExp
    pos <- getPosition
    return $ TmIf (infoFrom pos) cond t1 t2

parseVar :: LCParser
parseVar = do
    v <- ident
    ctx <- getState
    findVar v ctx

findVar :: String -> Context -> LCParser
findVar v ctx = case elemIndex (v, NameBind) ctx of
    Nothing -> fail $ "Can't find varible " ++ v
    Just n -> do
        pos <- getPosition
        return $ TmVar (infoFrom pos) n (length ctx)

-- Find a better way to parse boolean constants in parsec?

parseTrue :: LCParser
parseTrue = reserved "true" >> getPosition >>= \pos -> return $ TmTrue (infoFrom pos)

parseFalse :: LCParser
parseFalse = reserved "false" >> getPosition >>= \pos -> return $ TmFalse (infoFrom pos)

parseBool = parseTrue <|> parseFalse

factor :: LCParser
factor = try parseInt
    <|> try parseIf
    <|> try parseBool
    <|> try parseVar
    <|> try parseAbs
    <|> parens parseBinExp
    <|> parens parseExps

parseExps = do
    es <- many1 parseBinExp
    pos <- getPosition
    return $ foldl1 (TmApp $ infoFrom pos) es

contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

parseExp s = runParser (contents parseExps) [] "untyped" s

--runCtxParse p sn inp ctx = runIdentity $ runStateT

