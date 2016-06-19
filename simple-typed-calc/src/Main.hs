import System.Environment
import Syntax
import Parser

dummyInfo = Info 0 0

-- pretty print a term
showTerm :: Context -> Term -> String
showTerm ctx (TmVar info x n) =
    if length ctx == n
    then indexToName ctx x
    else "bad index"
showTerm ctx (TmAbs info str t1) =
    "(lambda " ++ str' ++ "." ++ (showTerm ctx' t1) ++ ")"
        where (ctx', str') = pickFreshName ctx str
showTerm ctx (TmApp info t1 t2) =
    "(" ++ (showTerm ctx t1) ++ " " ++ (showTerm ctx t2) ++ ")"
showTerm ctx t = show t

indexToName :: Context -> Int -> String
indexToName ctx idx = fst $ ctx !! idx

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx name =
    if name `elem` (map fst ctx)
    then pickFreshName ctx $ name ++ "'"
    else ((name, NameBind) : ctx, name)

shiftTerm :: Int -> Term -> Term
shiftTerm d t = walk 0 t
    where walk c (TmVar info x n) = if x >= c then TmVar info (x + d) (n + d) else TmVar info x (n + d)
          walk c (TmAbs info x t1) = TmAbs info x $ walk (c + 1) t1
          walk c (TmApp info t1 t2) = TmApp info (walk c t1) (walk c t2)
          -- Should be explicit about what goes next, this could totally be a 
          -- source of weird bugs
          walk c n = n

substTerm :: Int -> Term -> Term -> Term
substTerm idx toSub term = walk 0 term
    where walk c (TmVar info x n) = if x == idx + c then shiftTerm c toSub else TmVar info x n
          walk c (TmAbs info x t1) = TmAbs info x $ walk (c + 1) t1
          walk c (TmApp info t1 t2) = TmApp info (walk c t1) (walk c t2)
          walk c n = n

substTermTop :: Term -> Term -> Term
substTermTop s t = shiftTerm (-1) $ substTerm 0 (shiftTerm 1 s) t
 
getBinding info ctx idx = bindingShift (idx + 1) bind
    where (_, bind) = ctx !! idx

bindingShift i (TmAbbBind t) = TmAbbBind (shiftTerm i t)
bindingShift i b = b

isVal :: Context -> Term -> Bool
isVal ctx (TmTrue _) = True
isVal ctx (TmFalse _) = True
isVal ctx (TmInt _ _) = True
isVal ctx (TmAbs _ _ _) = True
isVal ctx (TmVar _ _ _) = True
isVal ctx (TmBinOp _ _ _ _) = True
isVal _ _ = False

isNumerical :: Term -> Bool
isNumerical (TmInt _ _) = True
isNumerical _ = False

eval' :: Context -> Term -> Term
eval' ctx (TmIf info cond t1 t2) = case eval' ctx cond of
    (TmTrue _) -> eval' ctx t1
    (TmFalse _) -> eval' ctx t2

eval' ctx t1@(TmApp info (TmAbs _ x t12) v2)
    | isVal ctx v2 = eval' ctx $ substTermTop v2 t12
eval' ctx (TmApp info v1 t2)
    | isVal ctx v1 = eval' ctx $ TmApp info v1 (eval' ctx t2)
eval' ctx (TmApp info t1 t2) = eval' ctx $ TmApp info (eval' ctx t1) t2

eval' ctx (TmVar info idx _) = case getBinding info ctx idx of
    (TmAbbBind t) -> eval' ctx t

eval' ctx (TmBinOp info op (TmInt _ n1) (TmInt _ n2)) = TmInt dummyInfo $ getOp op n1 n2
eval' ctx (TmBinOp info op t1 t2) = eval' ctx $ TmBinOp dummyInfo op (eval' ctx t1) (eval' ctx t2)

eval' ctx term = term

getOp :: Op -> (Int -> Int -> Int)
getOp Plus = (+)
getOp Times = (*)

process :: String -> IO ()
process inp = 
    -- Don't know how to deal with newlines properly in parsec, annoying
    putStrLn $ case parseExp inp of 
        Left err -> show err
        --Right parsed -> showTerm [] $ eval' [] parsed
        Right parsed -> show $ eval' [("x", (TmAbbBind (TmInt DummyInfo 55)))] parsed

main :: IO ()
main = do
    filePath <- fmap head getArgs
    inp <- readFile filePath
    putStrLn inp
    process inp
