import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Control.Monad.Except

import qualified Data.Text as T

import Top
import Syntax
import Eval

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

runTest :: String -> String
runTest code = case runExcept $ process code of
    (Right val) -> show val
    (Left err) -> show err

strip  = T.unpack . T.strip . T.pack

compareTestToResult :: Test -> String
compareTestToResult test = 
    if res == expected
    then "passed"
    else "expected: " ++ expected ++ "\nbut got: " ++ res
    where
        res = runTest (code test)
        expected = strip $ (result test)

main = do
    res <- parseTests "../testfiles"
    case res of
        Right tests -> mapM_ putStrLn $ map compareTestToResult tests
        Left err -> print err
