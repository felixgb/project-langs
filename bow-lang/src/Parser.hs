module Parser where

import Syntax

import Control.Monad.Except

import Text.Parsec
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

lexer = Tok.makeTokenParser style
    where
        ops = ["+", "*", "==", "=", ";", "|"]
        names = ["if", "then", "else", "fun", "let", "case", "of"]
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

-- ident = Tok.identifier lexer
ident = (Tok.lexeme lexer ci)
    where 
        ci = do
            start <- lower
            rest <- option [] (Tok.identifier lexer)
            return $ start : rest

commaSep = Tok.commaSep lexer

-- consIdent :: Parsec String () String
consIdent = (Tok.lexeme lexer ci)
    where 
        ci = do
            start <- upper
            rest <- option [] (Tok.identifier lexer)
            return $ start : rest

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

-- Parse Types

parseTyInt = reserved "Int" >> return TyInt

parseTyBool = reserved "Bool" >> return TyBool

parseTyUnit = reserved "Unit" >> return TyUnit

parseVariant = do
    constructors <- sepBy1 parseConstructor (reservedOp "|")
    return $ TyVariant constructors
    where
        parseConstructor = do
            name <- consIdent
            tys <- many parseType
            return (name, tys)

parseTyRec = do
    reserved "rec"
    tyVar <- consIdent
    reservedOp "->"
    ty <- parseType
    return $ TyRec tyVar ty

parseType = parseTyInt
    <|> parseTyBool
    <|> parseTyUnit
    <|> parseTyRec
    <|> parseVariant

-- Parse Expressions

parseDataDec = do
    pos <- getPosition
    reserved "data"
    name <- consIdent
    reservedOp "="
    ty <- parseType
    return $ EDataDec (infoFrom pos) name ty

parseCase = do
    pos <- getPosition
    reserved "case"
    expr <- factor
    reserved "of"
    cases <- sepBy1 (parens parseBranch) (reserved "|")
    return $ ECase (infoFrom pos) expr cases 
    where
        parseBranch = do
            pos <- getPosition
            ident <- consIdent
            vars <- parens $ commaSep parseVariable
            reservedOp "->"
            ex <- factor
            return ((ETag (infoFrom pos) ident vars), ex)

parseTag = do
    pos <- getPosition
    name <- consIdent
    vars <- parens $ commaSep factor
    return $ ETag (infoFrom pos) name vars

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

factor = try parseInt
    <|> try parseTag
    <|> try parseAssign
    <|> try parseBool
    <|> try parseDef
    <|> try parseInvoke
    <|> try parseIf
    <|> try parseDataDec
    <|> try parseCase
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

