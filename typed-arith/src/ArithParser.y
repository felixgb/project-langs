{
module ArithParser where

import ArithLexer
import Syntax
}

%name parseArith
%tokentype { Token }
%error { parseError }

%token
    if          { TIf }
    then        { TThen }
    else        { TElse }
    zero        { TZero }
    true        { TTrue }
    false       { TFalse }
    isZero      { TIsZero }
    succ        { TSucc }
    pred        { TPred }

%%

Exp : if Exp then Exp else Exp  { EIf $2 $4 $6 }
    | succ Exp                  { ESucc $2 }
    | pred Exp                  { EPred $2 }
    | isZero Exp                { EIsZero $2 }
    | zero                      { EZero }
    | true                      { ETrue }
    | false                     { EFalse }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
