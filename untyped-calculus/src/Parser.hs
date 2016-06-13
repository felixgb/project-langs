module Parser where

import Text.Parsec
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)

import Syntax

type LCParser = Parsec String Context Term

-- Get the line and column info from Parsec's SourcePos
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parseVarName :: Parsec String u String
parseVarName = many1 $ letter <|> char '\''

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda calculus"

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

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

parseNonApp :: LCParser
parseNonApp = parens parseTerm
    <|> parseAbs parseTerm
    <|> parseVar

parseTerm :: LCParser
parseTerm = chainl1 parseNonApp $ do
    space
    pos <- getPosition
    return $ TmApp (infoFrom pos)

parseLC :: String -> Either ParseError Term
parseLC = parseWith parseTerm
