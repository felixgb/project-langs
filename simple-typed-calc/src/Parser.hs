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

brackets = Tok.brackets lexer

ident = Tok.identifier lexer

commaSep = Tok.commaSep lexer

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

-- Types -----------------------------------------------------------------------

parseType = (reserved "Int" >> return TyInt) 
    <|> (reserved "Bool" >> return TyBool) 
    <|> (reserved "Unit" >> return TyUnit)
    <|> parseVariant
    <|> parseRecTy
    <|> parsePairType
    <|> (ident >>= \l -> return $ TyDataVar l)

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

parseRecTy = do
    reserved "Rec"
    tyVar <- ident
    reservedOp "->"
    ty <- parseType
    return $ TyRecTy tyVar ty

parsePairType = do
    tys <- parens $ commaSep parseType
    return $ TyProd tys

-- Terms -----------------------------------------------------------------------

parseInt :: LCParser
parseInt = do
    pos <- getPosition
    n <- Tok.integer lexer
    return $ TmInt (infoFrom pos) (fromIntegral n)

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
    pos <- getPosition
    reserved "case"
    t1 <- factor'
    reserved "of"
    cases <- sepBy1 caseAssocs $ reservedOp "|"
    return $ TmCase (infoFrom pos) t1 cases
    where caseAssocs = do
            reservedOp "<"
            l <- ident
            reservedOp "="
            x <- ident
            reservedOp ">"
            reservedOp "->"
            t <- factor'
            return $ (l, (x, t))

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

parseDataDec :: LCParser
parseDataDec = do
    reserved "data"
    name <- ident
    reservedOp "="
    varient <- parseType
    pos <- getPosition
    return $ TmDataDec (infoFrom pos) name varient

parsePair :: LCParser
parsePair = do
    pos <- getPosition
    tms <- parens $ commaSep factor'
    return $ TmPair (infoFrom pos) tms

parseProj :: LCParser
parseProj = do
    pos <- getPosition
    idx <- parseFst <|> parseSnd
    t <- factor'
    return $ TmProj (infoFrom pos) t idx
    where
        parseFst = (reserved "fst" >> return 0)
        parseSnd = (reserved "snd" >> return 1)

parseFold :: LCParser
parseFold = do
    pos <- getPosition
    reserved "fold"
    ty <- brackets parseType
    tm <- factor'
    return $ TmFold (infoFrom pos) ty tm

parseUnfold :: LCParser
parseUnfold = do
    pos <- getPosition
    reserved "unfold"
    ty <- brackets parseType
    tm <- factor'
    return $ TmUnfold (infoFrom pos) ty tm

parseTrue :: LCParser
parseTrue = reserved "true" >> getPosition >>= \pos -> return $ TmTrue (infoFrom pos)

parseFalse :: LCParser
parseFalse = reserved "false" >> getPosition >>= \pos -> return $ TmFalse (infoFrom pos)

parseBool = parseTrue <|> parseFalse

parseUnit = reserved "unit" >> getPosition >>= \pos -> return $ TmUnit (infoFrom pos)

factor' :: LCParser
factor' = (try $ parens parseExps)
    <|> parseProj
    <|> parseBool
    <|> parseInt
    <|> parseCase
    <|> parseTag
    <|> parseIf
    <|> parseAbs
    <|> parseUnit
    <|> parseDataDec
    <|> parseFold
    <|> parseUnfold
    <|> parseVar
    <|> (try parsePair)

parseExps = do
    es <- many1 term
    pos <- getPosition
    return $ foldl1 (TmApp $ infoFrom pos) es

parseTopLevel = do
    es <- sepBy1 term $ reservedOp ";"
    pos <- getPosition
    return $ foldr1 (\tm1 tm2 -> TmApp (infoFrom pos) (TmAbs DummyInfo "fresh!" TyUnit tm2) tm1) es

contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

parseExp s = runParser (contents parseTopLevel) () "untyped" s
