module Interp.Execution (
    execute,
    ) where

import Interp.Interp
import Interp.Eval
import ASTs.NonTerminals

execute :: Program -> String
execute p = case interp (eval p) env0 "" of
                 Left  (a,_,cs) -> cs
