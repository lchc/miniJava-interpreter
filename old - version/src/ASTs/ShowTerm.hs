module ASTs.ShowTerm (
    module ASTs.ShowTerm
    ) where

import ASTs.Terminals

instance Show Class   where show = const "(key class)"
instance Show Extends where show = const "(key extends)"
instance Show Public  where show = const "(key public)"
instance Show Static  where show = const "(key static)"
instance Show While   where show = const "(key while)"
instance Show If      where show = const "(key if)"
instance Show Else    where show = const "(key else)"
instance Show System  where show = const "(key System)"
instance Show Out     where show = const "(key out)"
instance Show Println where show = const "(key println)"
instance Show Return  where show = const "(key return)"
instance Show New     where show = const "(key new)"
instance Show KMain   where show = const "(key main)"
instance Show KString where show = const "(key String)"
instance Show Void    where show = const "(type void)"

instance Show LBracket where show = const "(op \"(\")"
instance Show RBracket where show = const "(op \")\")"
instance Show LBrace   where show = const "(op \"[\")"
instance Show RBrace   where show = const "(op \"]\")"
instance Show LCurly   where show = const "(op \"{\")"
instance Show RCurly   where show = const "(op \"}\")"
instance Show Comma    where show = const "(op \",\")"
instance Show Colon    where show = const "(op \";\")"
instance Show Dot      where show = const "(op \".\")"

instance Show AssignOp where show = const "(op \"=\")"
instance Show OrOp     where show = const "(op \"||\")"
instance Show AndOp    where show = const "(op \"&&\")"

instance Show AddOp
    where show Plus = "(op \"+\")"
          show Minus = "(op \"-\")"

instance Show EqOp
    where show Equal = "(op \"==\")"
          show NotEq = "(op \"!=\")"

instance Show RelOp
    where show Less    = "(op \"<\")"
          show Leq     = "(op \"<=\")"
          show Greater = "(op \">\")"
          show Geq     = "(op \">=\")"

instance Show MulOp
    where show Times  = "(op \"*\")"
          show Slash  = "(op \"/\")"
          show Modulo = "(op \"%\")"

instance Show UnaryOp
    where show No   = "(op \"!\")"
          show Nega = "(op \"-\")"

instance Show EOF where show = const "(EOF)"

instance Show Ident where
    show (Ident a) = "(id \"" ++ a ++ "\")"
