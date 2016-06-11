import System.Environment
import ArithParser
import ArithLexer
import Syntax

main :: IO ()
main = do
    strs <- getArgs
    process $ concat strs

process :: String -> IO ()
process inp = do
    let tokens = parseTokens inp
    let ast = parseExp inp
    case ast of
        Left err -> putStrLn err
        Right ast -> print $ typeOf ast

typeOf :: Exp -> Either String Type
typeOf ETrue = Right TyBool
typeOf EFalse = Right TyBool
typeOf (EIf t1 t2 t3) = case (typeOf t1) of
    Right TyBool -> if (ty2 == ty3) then ty2 else Left "Boolean expressions don't match"
        where ty2 = typeOf t2
              ty3 = typeOf t3
    Right _ -> Left $ (show t1) ++ " is not a boolean"
    err -> err
typeOf EZero = Right TyInt
typeOf (ESucc t1) = checkInt t1
typeOf (EPred t1) = checkInt t1
typeOf (EIsZero t1) = case (typeOf t1) of
    Right TyInt -> Right TyBool
    Right _ -> Left $ (show t1) ++ " is not a number"
    err -> err

checkInt :: Exp -> Either String Type
checkInt e = case (typeOf e) of
    (Right TyInt) -> Right TyInt
    (Right _) -> Left $ (show e) ++ "is not a number"
    err -> err

