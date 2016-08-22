import Text.Read
import System.Process

import Syntax

fa polys ty = Forall polys ty
int = TyInt
bool = TyBool
var name = TyVar name
func ins out = TyFunc ins out

typeTestsResults :: [(Int, Type)]
typeTestsResults =
    [ (0, fa [] int)
    , (1, fa [] $ func [] int)
    , (2, fa [] $ func [int] int)
    , (3, fa [] int)
    , (4, fa [] bool)
    , (5, fa [] int)
    , (6, fa [] int)
    , (7, fa [] int)
    , (8, fa [var "a1"] $ func [var "a1"] (var "a1"))
    , (9, fa [] $ func [int] (func [int] int))
    , (10, fa [] int)
    , (11, fa [] int)
    , (12, fa [] bool)
    , (13, fa [var "a1"] $ func [] (func [var "a1"] (var "a1")))
    , (14, fa [] $ func [] int)
    , (15, fa [] $ func [] int)
    ]

runTypeTest :: (Int, Type) -> IO ()
runTypeTest (num, expectedRes) = do
    let testPath = "../tests/test" ++ (show num) ++ ".bow"
    res <- readProcess "../dist/build/bow-lang/bow-lang" ["--type", testPath] ""
    putStrLn $ "Test number " ++ (show num) ++ ", : " ++ case readEither res of
        Right ty -> getRes ty
        Left err -> show err
    where 
        getRes r = if r == expectedRes
            then "ok"
            else "failed! \n\tExpected: \n\t" ++ (show expectedRes) ++ "\n\tbut got: \n\t" ++ (show r)

main = mapM_ runTypeTest typeTestsResults
