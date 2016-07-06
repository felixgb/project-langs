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

data Test = Test { path :: String
                 , result :: String
                 } deriving (Eq, Show)

lexer = Tok.makeTokenParser style
    where
        ops = [":"]
        names = ["result", "path"]
        style = emptyDef {
            Tok.commentLine = "--"
            , Tok.reservedOpNames = ops
            , Tok.reservedNames = names
            }

reservedOp = Tok.reservedOp lexer

reserved = Tok.reserved lexer

line w = do
    reserved w
    reservedOp ":"
    result <- many (noneOf "\n")
    many (newline <|> comment)
    return result

parseCodeTest = do
    result <- line "result"
    path <- line "path"
    return $ Test path result

comment = do
    reservedOp "--"
    manyTill anyChar newline
    return 'x'

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

strip = T.unpack . T.strip . T.pack

compareTestToResult :: (String, String) -> String
compareTestToResult test = 
    if res == expected
    then "passed"
    else "expected: " ++ expected ++ "\nbut got: " ++ res
    where
        res = runTest (fst test)
        expected = strip $ (snd test)

main = do
    res <- parseTests "../testfiles"
    case res of
        Right tests -> do
            codes <- mapM ((\p -> readFile $ "../test_code/" ++ p) . path) tests
            let results = map result tests
            mapM_ putStrLn $ map compareTestToResult $ zip codes results
        Left err -> print err
