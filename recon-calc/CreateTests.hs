import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Top
import System.Directory

type Code = String
type Result = String

data Test = Test { code :: String
                 , result :: String
                 } deriving (Eq, Show)

lexer = Tok.makeTokenParser style
    where
        ops = [":"]
        names = ["result", "code"]
        style = emptyDef {
            Tok.commentLine = "--"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames = names
            }

reservedOp = Tok.reservedOp lexer

reserved = Tok.reserved lexer

tillMark = do
    p <- manyTill (noneOf "!") (reserved "!")
    return p

parseCodeTest = do
    reserved "result"
    reservedOp ":"
    result <- tillMark
    reserved "code"
    reservedOp ":"
    code <-  tillMark
    return $ Test code result

contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

parseTests path = parseFromFile (contents $ many parseCodeTest) path

runTest :: 

main = do
    res <- parseTests "../testfiles"
    case res of
        Right tests -> print tests
        Left err -> print err
