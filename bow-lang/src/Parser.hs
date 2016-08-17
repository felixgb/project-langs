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
        names = ["if", "then", "else", "def", "let", "case", "of"]
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

stringLit = Tok.stringLiteral lexer

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
        ,[binary "-" Minus Ex.AssocLeft]
        ,[binary "==" Equal Ex.AssocLeft]]

expr = Ex.buildExpressionParser table factor

-- Parse Types

parseTyInt = reserved "Int" >> return TyInt

parseTyBool = reserved "Bool" >> return TyBool

parseTyUnit = reserved "Unit" >> return TyUnit

parseTyVar = do
    tyVar <- consIdent
    return $ TyVar tyVar

parseTyRec = do
    reserved "rec"
    tyVar <- consIdent
    reservedOp "->"
    ty <- parseType
    return $ TyRec tyVar ty

parseTyApp = do
    tys <- many1 parseType
    return $ foldr1 TyApp tys

parseTyDec = do
    reserved "type"
    name <- consIdent
    params <- many $ parseTyVar
    reservedOp "="
    ty <- parseType
    return $ ETyDef name params ty

parseTyUnion = do
    constructors <- braces $ sepBy1 parseConstructor (reservedOp "|")
    return $ TyTaggedUnion constructors
    where
        parseConstructor = do
            name <- consIdent
            tys <- parens $ commaSep parseType
            return (name, tys)

parseTyRecord = do
    fields <- braces $ commaSep parseField
    return $ TyRecord fields
    where
        parseField = do
            name <- consIdent
            reservedOp ":"
            ty <- parseType
            return (name, ty)

parseType = try parseTyRecord
    <|> try parseTyUnion
    <|> parseTyUnit
    <|> parseTyInt
    <|> parseTyBool
    <|> parseTyVar
    <|> parens parseTyApp

-- parseType = parseTyInt
--     <|> parseTyBool
--     <|> parseTyUnit
--     <|> parseTyRec
--     <|> try parseTyUnion
--     <|> try parseTyVar
-- 
-- Parse Expressions

parseTaggedUnion = do
    pos <- getPosition
    reserved "tagged"
    name <- consIdent
    ty <- braces parseType
    return $ ETaggedUnion (infoFrom pos) name (insertRec name ty)

insertRec :: String -> Type -> Type
insertRec name ty = if name `occursIn` ty 
    then TyRec "L" (substType name (TyVar "L") ty)
    else ty

occursIn :: String -> Type -> Bool
occursIn tyName ty = occin ty
    where
        occin (TyFunc tys ty2) = (all occin tys) || occin ty2
        occin TyInt = False
        occin TyBool = False
        occin TyUnit = False
        occin (TyVar s) = s == tyName
        occin (TyTaggedUnion fts) = any occin $ concatMap snd fts


substType :: String -> Type -> Type -> Type
substType tyName tyT tyS = st tyS
    where
        st (TyFunc tys tyS2) = TyFunc (fmap st tys) (st tyS2)
        st TyInt = TyInt
        st TyBool = TyBool
        st TyUnit = TyUnit
        st (v@(TyVar name)) = if name == tyName then tyT else v
        st (TyTaggedUnion fts) = (TyTaggedUnion (map substed fts))
            where
                substed (name, tys) = (name, map st tys)

parseCase = do
    pos <- getPosition
    reserved "case"
    expr <- factor
    reserved "of"
    cases <- braces $ many parseBranch 
    return $ ECase (infoFrom pos) expr cases 
    where
        parseBranch = do
            pos <- getPosition
            ident <- consIdent
            vars <- parens $ commaSep factor
            reservedOp "->"
            ex <- factor
            reservedOp ";"
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

parseString = do
    pos <- getPosition
    str <- stringLit
    return $ ELit (infoFrom pos) (LString str)

parseDef = do
    pos <- getPosition
    reserved "def"
    name <- ident
    params <- parens $ commaSep ident
    body <- braces $ sepBy factor (reservedOp ";") 
    return $ EDef (infoFrom pos) name params (foldr1 ESeq body)

parseCallShell = do
    pos <- getPosition
    reserved "sh"
    (command, args) <- parens inner
    return $ ECallShell (infoFrom pos) command args
    where 
        inner = do
            c <- factor
            reservedOp ","
            as <- factor
            return (c, as)

parsePrint = do
    pos <- getPosition
    reserved "print"
    arg <- parens factor
    return $ EPrint (infoFrom pos) arg

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

parseFor = do
    pos <- getPosition
    reserved "for"
    iter <- factor
    reserved "in"
    seq <- factor
    body <- braces $ sepBy factor (reservedOp ";")
    return $ EFor (infoFrom pos) iter seq (foldr1 ESeq body)

parseVec = do
    pos <- getPosition
    reserved "vec"
    elems <- brackets $ commaSep factor
    return $ EVector (infoFrom pos) (V.fromList elems)

parseIndex = do
    pos <- getPosition
    name <- ident
    idx <- brackets factor
    return $ EIndex (infoFrom pos) name idx

parseTrue = reserved "true" >> (return $ LBool True)

parseFalse = reserved "false" >> (return $ LBool False)

parseUnit = reserved "unit" >> (return $ EUnit DummyInfo)

parseBool = do
    pos <- getPosition
    bool <- parseTrue <|> parseFalse
    return $ ELit (infoFrom pos) bool

factor = try parseInt
    <|> try parseTyDec
    <|> try parseUnit
    <|> try parseString
    <|> try parseFor
    <|> try parseCallShell
    <|> try parsePrint
    <|> try parseVec
    <|> try parseIndex
    <|> try parseTag
    <|> try parseAssign
    <|> try parseBool
    <|> try parseDef
    <|> try parseInvoke
    <|> try parseIf
    <|> try parseTaggedUnion
    <|> try parseCase
    <|> try parseVariable
    <|> parens expr

parseDefs = do 
    defs <- many (factor)
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

