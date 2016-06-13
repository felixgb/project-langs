import System.Environment
import Control.Monad.Except
import Syntax
import Parser

-- Should be an instance of Show instead...
showTerm :: Context -> Term -> String
showTerm ctx (TmVar info x n) =
    if length ctx == n
    then indexToName info ctx x
    else "bad index"
showTerm ctx (TmAbs info str t1) =
    "(lambda " ++ str' ++ "." ++ (showTerm ctx' t1) ++ ")"
        where (ctx', str') = pickFreshName ctx str
showTerm ctx (TmApp info t1 t2) =
    "(" ++ (showTerm ctx t1) ++ " " ++ (showTerm ctx t2) ++ ")"

indexToName info ctx x = undefined
pickFreshName ctx x = undefined

shiftTerm :: Int -> Term -> Term
shiftTerm d t = walk 0 t
    where walk c (TmVar info x n) = if x >= c then TmVar info (x + d) (n + d) else TmVar info x (n + d)
          walk c (TmAbs info x t1) = TmAbs info x $ walk (c + 1) t1
          walk c (TmApp info t1 t2) = TmApp info (walk c t1) (walk c t2)

substTerm :: Int -> Term -> Term -> Term
substTerm idx toSub term = walk 0 term
    where walk c (TmVar info x n) = if x == idx + c then shiftTerm c toSub else TmVar info x n
          walk c (TmAbs info x t1) = TmAbs info x $ walk (c + 1) t1
          walk c (TmApp info t1 t2) = TmApp info (walk c t1) (walk c t2)

substTermTop :: Term -> Term -> Term
substTermTop s t = shiftTerm (-1) $ substTerm 0 (shiftTerm 1 s) t

isVal :: Context -> Term -> Bool
isVal ctx (TmAbs _ _ _) = True
isVal ctx _ = False

eval :: Context -> Term -> Maybe Term
eval ctx (TmApp info (TmAbs _ x t12) v2@(TmAbs _ _ _)) = Just $ substTermTop v2 t12
eval ctx (TmApp info v1@(TmAbs _ _ _) t2) = do 
    t2' <- eval ctx t2
    return $ TmApp info v1 t2'
eval ctx (TmApp info t1 t2) = do
    t1' <- eval ctx t1
    return $ TmApp info t1' t2
eval _ _ = Nothing

runEval :: Context -> Term -> Term
runEval ctx t = case (eval ctx t) of
    Nothing -> t
    Just next -> runEval ctx next

main :: IO ()
main = do
    filePath <- fmap head getArgs
    inp <- readFile filePath
    putStrLn inp
    process inp

process :: String -> IO ()
process inp = 
    putStrLn $ case parseLC $ filter (/= '\n') inp of -- Don't know how to deal with this in parsec
        Left err -> show err
        Right parsed -> show $ eval [] parsed

--eval :: Exp -> Exp 
--eval (EIf ETrue e2 e3) = eval e2
--eval (EIf EFalse e2 e3) = eval e3
--eval (EIf t1 t2 t3) = let t1' = eval t1 in eval $ EIf t1' t2 t3
--eval (ESucc t1) = ESucc $ eval t1
--eval (ESucc EZero) = EZero
--eval (EPred EZero) = EZero
--eval (EPred (ESucc nv)) = nv
--eval (EPred t1) = let t1' = eval t1 in eval $ EPred t1'
--eval (EIsZero EZero) = eval $ ETrue
--eval (EIsZero (ESucc nv)) = EFalse
--eval (EIsZero t1) = EIsZero $ eval t1
--eval EZero = EZero
--eval ETrue = ETrue
--eval EFalse = EFalse
--
--typeOf :: Exp -> Either String Type
--typeOf ETrue = Right TyBool
--typeOf EFalse = Right TyBool
--typeOf (EIf t1 t2 t3) = case (typeOf t1) of
--    Right TyBool -> if (ty2 == ty3) then ty2 else Left "Expressions don't match"
--        where ty2 = typeOf t2
--              ty3 = typeOf t3
--    Right _ -> Left $ (show t1) ++ " is not a boolean"
--    err -> err
--typeOf EZero = Right TyInt
--typeOf (ESucc t1) = checkInt t1
--typeOf (EPred t1) = checkInt t1
--typeOf (EIsZero t1) = case (typeOf t1) of
--    Right TyInt -> Right TyBool
--    Right _ -> Left $ (show t1) ++ " is not a number"
--    err -> err
--
--checkInt :: Exp -> Either String Type
--checkInt e = case (typeOf e) of
--    (Right TyInt) -> Right TyInt
--    (Right _) -> Left $ (show e) ++ "is not a number"
--    err -> err
--
