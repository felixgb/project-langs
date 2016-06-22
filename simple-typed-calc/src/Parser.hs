module Parser (
    parseExp
) where

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

type LCParser = Parsec String () Term

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

binary s f assoc = Ex.Infix (opInfo s f) assoc
    where opInfo name op = do
            reservedOp name
            pos <- getPosition
            return $ TmBinOp (infoFrom pos) op

table = [[binary "*" Times Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft]]

term :: LCParser
term = Ex.buildExpressionParser table factor'

parseInt :: LCParser
parseInt = do
    pos <- getPosition
    n <- Tok.integer lexer
    return $ TmInt (infoFrom pos) (fromIntegral n)

--parseType :: LCParser
parseType = (reserved "Int" >> return TyInt) 
    <|> (reserved "Bool" >> return TyBool) 
    <|> parseVariant

parseAbs :: LCParser
parseAbs = do
    reservedOp "\\"
    v <- ident
    reservedOp ":"
    ty <- parseType
    reservedOp "->"
    term <- parseExps
    pos <- getPosition
    return $ TmAbs (infoFrom pos) v ty term

parseVariant = do
    reservedOp "<"
    vts <- sepBy1 assoc $ reservedOp ","
    reservedOp ">"
    return $ TyVariant vts
    where assoc = do
            l <- ident
            reservedOp ":"
            ty <- parseType
            return (l, ty)

parseTag :: LCParser
parseTag = do
    reservedOp "<"
    l <- ident
    reservedOp "="
    t <- factor'
    reservedOp ">"
    reserved "as"
    ty <- parseType
    pos <- getPosition
    return $ TmTag (infoFrom pos) l t ty

parseCase :: LCParser
parseCase = do
    reserved "case"

parseIf :: LCParser
parseIf = do
    reserved "if"
    cond <- factor'
    reserved "then"
    t1 <- factor'
    reserved "else"
    t2 <- factor'
    pos <- getPosition
    return $ TmIf (infoFrom pos) cond t1 t2

parseVar :: LCParser
parseVar = do
    v <- ident
    pos <- getPosition
    return $ TmVar (infoFrom pos) v

parseTrue :: LCParser
parseTrue = reserved "true" >> getPosition >>= \pos -> return $ TmTrue (infoFrom pos)

parseFalse :: LCParser
parseFalse = reserved "false" >> getPosition >>= \pos -> return $ TmFalse (infoFrom pos)

parseBool = parseTrue <|> parseFalse

factor' :: LCParser
factor' = parens parseExps
    <|> parseBool
    <|> parseInt
    <|> parseIf
    <|> parseAbs
    <|> parseVar

parseExps = do
    es <- many1 term
    pos <- getPosition
    return $ foldl1 (TmApp $ infoFrom pos) es

contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

parseExp s = runParser (contents parseExps) () "untyped" s
