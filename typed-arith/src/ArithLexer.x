{
{-# LANGUAGE FlexibleContexts #-}

module ArithLexer where

import Control.Monad.Except
}

%wrapper "posn"

tokens :-

    $white+     ;
    if          { \p s -> (TIf, p) }
    then        { \p s -> (TThen, p) }
    else        { \p s -> (TElse, p) }
    0           { \p s -> (TZero, p) }
    true        { \p s -> (TTrue, p) }
    false       { \p s -> (TFalse, p) }
    isZero      { \p s -> (TIsZero, p) }
    succ        { \p s -> (TSucc, p) }
    pred        { \p s -> (TPred, p) }

{
type TokPos = (Token, (Int, Int))

data Token 
    = TIf
    | TThen
    | TElse
    | TZero
    | TTrue
    | TFalse
    | TSucc
    | TPred
    | TIsZero
    deriving (Eq, Show)

scanTokens :: String -> Except String [(Token, AlexPosn)]
scanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_,_) -> throwError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> do
                    res <- go inp'
                    let rest = act pos (take len str)
                    return (rest : res)

--convertedTokens :: String -> Except String [TokPos]
--convertedTokens str = (scanTokens str) >>= (\ts -> map (\(t, (AlexPn _ l c)) -> (t, (l, c))) ts)

convertedTokens :: String -> Except String [TokPos]
convertedTokens str = do
    toks <- (scanTokens str)
    return $ map (\(t, (AlexPn _ l c)) -> (t, (l, c))) toks
    
}
