module Parser where

import Syntax

import Control.Monad.Except

import Text.Parsec
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

lexer = Tok.makeTokenParser style
    where
        ops = ["+", "*", "==", "=", ";"]
        names = ["if", "then", "else", "fun", "let"]
        style = emptyDef {
            Tok.commentLine = "//"
            , Tok.reservedNames = names
            , Tok.reservedOpNames = ops
            }

reservedOp = Tok.reservedOp lexer

reserved = Tok.reserved lexer

parens = Tok.parens lexer

brackets = Tok.brackets lexer

braces = Tok.braces lexer

ident = Tok.identifier lexer

commaSep = Tok.commaSep lexer

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

binary s f assoc = Ex.Infix (opInfo s f) assoc
    where 
        opInfo name op = do
            reservedOp name
            pos <- getPosition
            return $ EBinexp (infoFrom pos) op

table = [[binary "*" Times Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft]
        ,[binary "==" Equal Ex.AssocLeft]]

expr = Ex.buildExpressionParser table factor

-- Parse Expressions

parseVariable = do
    pos <- getPosition
    name <- ident
    return $ EVar (infoFrom pos) name

parseAssign = do
    pos <- getPosition
    reserved "let"
    name <- ident
    reserved "="
    expr <- factor
    return $ EAssign (infoFrom pos) name expr

parseInt = do
    pos <- getPosition
    n <- Tok.integer lexer
    return $ ELit (infoFrom pos) (LInt $ fromIntegral n)

parseDef = do
    pos <- getPosition
    reserved "fun"
    name <- ident
    params <- parens $ commaSep ident
    body <- braces $ sepBy factor (reservedOp ";") 
    return $ EDef (infoFrom pos) name params (foldr1 ESeq body)

parseInvoke = do
    pos <- getPosition
    name <- ident
    args <- parens $ commaSep factor
    return $ EInvoke (infoFrom pos) name args

parseIf = do
    pos <- getPosition
    reserved "if"
    cond <- factor
    reserved "then"
    tr <- factor
    reserved "else"
    fl <- factor
    return $ EIf (infoFrom pos) cond tr fl

parseTrue = reserved "true" >> (return $ LBool True)

parseFalse = reserved "false" >> (return $ LBool False)

parseBool = do
    pos <- getPosition
    bool <- parseTrue <|> parseFalse
    return $ ELit (infoFrom pos) bool

-- 
-- parseBool = do
--     pos <- getPosition
--     b <- Tok.boolean lexer
--     return $ ELit (infoFrom pos) (LBool b)
-- 

factor = try parseInt
    <|> try parseAssign
    <|> try parseBool
    <|> try parseDef
    <|> try parseInvoke
    <|> try parseIf
    <|> try parseVariable
    <|> parens expr

parseDefs = do 
    defs <- sepBy factor (reservedOp ";")
    return defs


contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

toplevel = do
    defs <- parseDefs
    return $ foldr1 ESeq defs

parseTopLevel :: String -> ThrowsError Expr
parseTopLevel s = case parse (contents toplevel) "<stdin>" s of
    Right ast -> return ast
    Left err -> throwError $ ErrParse (show err)

