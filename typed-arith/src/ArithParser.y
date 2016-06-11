{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArithParser where

import ArithLexer
import Syntax

import Control.Monad.Except
import Control.Monad.Trans.Except
}

%name parseArith
%tokentype { TokPos }
%monad { Except String } { (>>=) } { return }
%error { parseError }

%token
    if          { (TIf, _) }
    then        { (TThen, _) }
    else        { (TElse, _) }
    zero        { (TZero, _) }
    true        { (TTrue, _) }
    false       { (TFalse, _) }
    isZero      { (TIsZero, _) }
    succ        { (TSucc, _) }
    pred        { (TPred, _) }

%%

Exp : if Exp then Exp else Exp  { EIf $2 $4 $6 }
    | succ Exp                  { ESucc $2 }
    | pred Exp                  { EPred $2 }
    | isZero Exp                { EIsZero $2 }
    | zero                      { EZero }
    | true                      { ETrue }
    | false                     { EFalse }

{

parseError :: [TokPos] -> Except String a
parseError ls = throwError (formatErrors ls)
parseError [] = throwError "Unexpected end of Input"

formatErrors :: [TokPos] -> String
formatErrors errs = concatMap errMsg errs
    where errMsg (tok, (ln, col)) = "Parse error at line " ++ (show ln) ++ " column " ++ (show col) ++ "\n"

parseExp :: String -> Either String Exp
parseExp input = runExcept $ do
    tokenStream <- convertedTokens input
    parseArith tokenStream

parseTokens :: String -> Either String [TokPos]
parseTokens = runExcept . convertedTokens

}
