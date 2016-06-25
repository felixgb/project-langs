module Parser where

import Syntax

import Text.Parsec

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        ops = ["+", "*", "-", ";"]
        names = ["def", "extern"]
        style = emptyDef {
              Tok.commentLine = "#"
            , Tok.reservedNames = names
            , Tok.reservedOpNames = ops
            }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

ident :: Parser String
ident = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- parser functions

binary opName assoc = Ex.Infix (reservedOp opName >> return (BinaryOp opName)) assoc

table = [[binary "*" Ex.AssocLeft,
          binary "/" Ex.AssocLeft]
        ,[binary "+" Ex.AssocLeft,
          binary "-" Ex.AssocLeft]]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

int :: Parser Expr
int = integer >>= \n -> return $ Float (fromInteger n)

floating :: Parser Expr
floating = float >>= \n -> return $ Float n

variable :: Parser Expr
variable = ident >>= \l -> return $ Var l

function :: Parser Expr
function = do
    reserved "def"
    name <- ident
    args <- parens $ commaSep ident
    body <- expr
    return $ Function name args body

extern :: Parser Expr
extern = do
    reserved "extern"
    name <- ident
    args <- parens $ commaSep ident
    return $ Extern name args

call :: Parser Expr
call = do
    name <- ident
    args <- parens $ commaSep expr
    return $ Call name args

factor :: Parser Expr
factor = try floating
    <|> try int
    <|> try extern
    <|> try function
    <|> try call
    <|> variable
    <|> parens expr

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

topLevel :: Parser [Expr]
topLevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s = parse (contents topLevel) "<stdin>" s
