import System.Environment
import Syntax
import Parser

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

substTerm :: Int -> Term -> Term -> Term
substTerm idx toSub term = walk 0 term
    where walk c (TmVar info x n) = if x == idx + c then shiftTerm c toSub else TmVar info x n
          walk c (TmAbs info x t1) = TmAbs info x $ walk (c + 1) t1
          walk c (TmApp info t1 t2) = TmApp info (walk c t1) (walk c t2)

substTermTop :: Term -> Term -> Term
substTermTop s t = shiftTerm (-1) $ substTerm 0 (shiftTerm 1 s) t

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

process :: String -> IO ()
process inp = 
    -- Don't know how to deal with newlines properly in parsec, annoying
    putStrLn $ case parseLC $ filter (/= '\n') inp of 
        Left err -> show err
        Right parsed -> showTerm [] $ runEval [] parsed

main :: IO ()
main = do
    filePath <- fmap head getArgs
    inp <- readFile filePath
    putStrLn inp
    process inp
