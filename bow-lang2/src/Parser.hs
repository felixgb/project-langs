module Parser where

import Syntax

import Control.Monad.Except

import qualified Data.Vector as V

import Text.Parsec
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

lexer = Tok.makeTokenParser style
    where
        ops = ["+", "*", "-", "==", "=", ";", "|"]
        names = ["call", "if", "then", "else", "def", "let", "case", "of"]
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

angles = Tok.angles lexer

stringLit = Tok.stringLiteral lexer

commaSep = Tok.commaSep lexer

-- ident = Tok.identifier lexer
ident = (Tok.lexeme lexer ci)
    where 
        ci = do
            start <- lower
            rest <- option [] (Tok.identifier lexer)
            return $ start : rest

-- tyIdent :: Parsec String () String
tyIdent = (Tok.lexeme lexer ci)
    where 
        ci = do
            start <- upper
            rest <- option [] (Tok.identifier lexer)
            return $ start : rest

binary s f assoc = Ex.Infix (reservedOp s >> return (EBinop f)) assoc
    where 
        opInfo name op = reservedOp name >> return op

table = [[binary "*" Times Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft]
        ,[binary "-" Minus Ex.AssocLeft]
        ,[binary "==" Equal Ex.AssocLeft]]

expr = Ex.buildExpressionParser table factor

-- Parse Types

parseTyInt = reserved "Int" >> (return $ TyApp TyInt [])

parseTyBool = reserved "Bool" >> (return $ TyApp TyInt [])

parseTyUnit = reserved "Unit" >> (return $ TyApp TyUnit [])

parseTyVar = do
    name <- tyIdent
    return $ TyVar name

parseTyFunc = do
    argTys <- brackets $ commaSep parseType
    reservedOp "->"
    retTy <- parseType
    return $ TyApp TyArrow (argTys ++ [retTy])

parsePoly = do
    reserved "forall"
    vars <- angles $ commaSep tyIdent
    ty <- parseType
    return $ TyPoly vars ty

parseType = try parseTyInt
    <|> try parseTyBool
    <|> try parseTyUnit
    <|> try parseTyFunc
    <|> try parsePoly
    <|> try parseTyVar

-- Parse Expressions

parseInt = do
    n <- Tok.integer lexer
    return $ EInt (fromIntegral n)

parseTrue = reserved "true" >> (return True)

parseFalse = reserved "false" >> (return False)

parseBool = do
    bool <- parseTrue <|> parseFalse
    return $ EBool bool

parseVariable = do
    name <- ident
    return $ EVar name

parseAssign = do
    reserved "let"
    (name, ty) <- tyField
    reserved "="
    expr <- factor
    return $ EAssign name ty expr

parseInvoke = do
    reserved "call"
    name <- factor
    tys <- angles $ commaSep parseType
    args <- parens $ commaSep factor
    return $ EInvoke name tys args

parseSeq = do
    exprs <- braces $ sepBy factor (reservedOp ";")
    return $ foldr1 ESeq exprs

factor = try parseInt
    <|> try parseBool
    <|> try parseSeq
    <|> try parseAssign
    <|> try parseInvoke
    <|> try parseVariable
    <|> parens expr

tyField = do
    name <- ident
    reservedOp ":"
    ty <- parseType
    return (name, ty)

parseFuncDef = do
    reserved "def"
    name <- ident
    polyTys <- angles $ commaSep tyIdent
    args <- parens $ commaSep tyField
    reservedOp ":"
    retTy <- parseType
    reservedOp "="
    body <- factor
    return $ FunDecl name polyTys args retTy body

parseDecl = try parseFuncDef

contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

topLevel = many $ do
    def <- parseDecl
    return def

parseTopLevel :: String -> ThrowsError Expr
parseTopLevel s = case parse (contents topLevel) "<stdin>" s of
    Right ast -> return $ foldr1 ESeq ast
    Left err -> throwError $ ErrParse (show err)

