module Parser where

import Text.Parsec
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Data.List (elemIndex)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Syntax

type LCParser = Parsec String Context Term

langaugeDef = emptyDef { Tok.commentStart = "//"
    , Tok.reservedNames = [ "if"
        , "then"
        , "else"
        ]
    , Tok.reservedOpNames = [ "\\"
    ]
    }

--binary s f assoc = Ex.Infix (reservedOp s >> return (TmBinOp f)) assoc
binary s f assoc = Ex.Infix (opInfo s f) assoc
    where opInfo name op = do
            reservedOp name
            pos <- getPosition
            return $ TmBinOp (infoFrom pos) op

table = [
    [ binary "*" Times Ex.AssocLeft
    , binary "+" Plus Ex.AssocLeft
    ]
    ]

lexer = Tok.makeTokenParser langaugeDef

parens :: Parsec String u a -> Parsec String u a
parens = Tok.parens lexer

--reserved :: String -> Parser ()
reserved = Tok.reserved lexer

--reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

opers :: LCParser
opers = Ex.buildExpressionParser table parseTerm

-- Get the line and column info from Parsec's SourcePos
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parseVarName :: Parsec String u String
parseVarName = many1 $ letter <|> char '\''

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda calculus"

parseInt :: LCParser
parseInt = do
    pos <- getPosition
    n <- Tok.integer lexer
    return $ TmInt (infoFrom pos) (fromIntegral n)

-- Parsing booleans could be made more general?
parseTrue :: LCParser
parseTrue = do
    pos <- getPosition
    reserved "true"
    return $ TmTrue (infoFrom pos)

parseFalse :: LCParser
parseFalse = do
    pos <- getPosition
    reserved "false"
    return $ TmFalse (infoFrom pos)

parseBool :: LCParser
parseBool = parseFalse <|> parseTrue

parseAbs :: LCParser -> LCParser
parseAbs termParser = do
    char '\\'
    v <- parseVarName
    modifyState ((v, NameBind) :)   -- Prepend variable name onto context
    char '.'
    term <- parseTerm
    modifyState tail    -- Leaving scope of the abstraction, pop variable off context
    pos <- getPosition
    return $ TmAbs (infoFrom pos) v term

parseVar :: LCParser
parseVar = do
    v <- parseVarName
    ctx <- getState
    findVar v ctx

findVar :: String -> Context -> LCParser
findVar v ctx = case elemIndex (v, NameBind) ctx of
    -- Can't deal with free variables, just fails instead
    Nothing -> fail $ "Can't find variable " ++ v
    Just n -> do
        pos <- getPosition
        return $ TmVar (infoFrom pos) n (length ctx)

parseIfThenElse :: LCParser
parseIfThenElse = do
    pos <- getPosition
    reserved "if"
    guard <- parseTerm
    reserved "then"
    e1 <- parseTerm
    reserved "else"
    e2 <- parseTerm
    return $ TmIf (infoFrom pos) guard e1 e2

parseNonApp :: LCParser
parseNonApp = parens parseTerm
    <|> parseBool
    <|> parseInt
    <|> parseIfThenElse
    <|> parseAbs parseTerm
    <|> parseVar

parseTerm :: LCParser
parseTerm = chainl1 parseNonApp $ do
    space
    pos <- getPosition
    return $ TmApp (infoFrom pos)

parseLC :: String -> Either ParseError Term
parseLC = parseWith opers
--parseLC = parseWith parseTerm
