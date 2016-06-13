import System.Environment
import Control.Monad.Except
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
        Right ast -> print $ case typeOf ast of
            Left err -> err
            Right checked -> show $ eval ast

eval :: Exp -> Exp 
eval (EIf ETrue e2 e3) = eval e2
eval (EIf EFalse e2 e3) = eval e3
eval (EIf t1 t2 t3) = let t1' = eval t1 in eval $ EIf t1' t2 t3
eval (ESucc t1) = ESucc $ eval t1
eval (ESucc EZero) = EZero
eval (EPred EZero) = EZero
eval (EPred (ESucc nv)) = nv
eval (EPred t1) = let t1' = eval t1 in eval $ EPred t1'
eval (EIsZero EZero) = eval $ ETrue
eval (EIsZero (ESucc nv)) = EFalse
eval (EIsZero t1) = EIsZero $ eval t1
eval EZero = EZero
eval ETrue = ETrue
eval EFalse = EFalse

typeOf :: Exp -> Either String Type
typeOf ETrue = Right TyBool
typeOf EFalse = Right TyBool
typeOf (EIf t1 t2 t3) = case (typeOf t1) of
    Right TyBool -> if (ty2 == ty3) then ty2 else Left "Expressions don't match"
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

