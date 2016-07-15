import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Control.Monad.Except

import System.Process

import qualified Data.Text as T

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

runTest :: String -> IO String
runTest path = readProcess "runhaskell" ["Main.hs", path] ""

strip = T.unpack . T.strip . T.pack

main = do
    res <- parseTests "../test/test_results.txt"
    case res of
        Right tests -> do
            out <- mapM (\test -> runTest $ "../test/" ++ (path test)) tests
            mapM_ putStrLn out
