{
module ArithLexer where
}

%wrapper "basic"

tokens :-

    $white+     ;
    if          { \s -> TIf }
    then        { \s -> TThen }
    else        { \s -> TElse }
    0           { \s -> TZero }
    true        { \s -> TTrue }
    false       { \s -> TFalse }
    isZero      { \s -> TIsZero }
    succ        { \s -> TSucc }
    pred        { \s -> TPred }

{
data Token = TIf
    | TThen
    | TElse
    | TZero
    | TTrue
    | TFalse
    | TSucc
    | TPred
    | TIsZero
    deriving (Show)

lexArith = alexScanTokens
}
