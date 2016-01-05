module ASTs.Terminals (
    module ASTs.Terminals
    ) where

data Class    = Class
data Extends  = Extends
data Public   = Public
data Static   = Static
data While    = While
data If       = If
data Else     = Else
data System   = System
data Out      = Out
data Println  = Println
data Return   = Return
data New      = New
data KMain    = KMain
data KString  = KString
data Void     = Void

data LBracket = LBracket
data RBracket = RBracket
data LBrace   = LBrace
data RBrace   = RBrace
data LCurly   = LCurly
data RCurly   = RCurly
data Comma    = Comma
data Colon    = Colon
data Dot      = Dot

data AssignOp = AssignOp
data OrOp     = OrOp
data AndOp    = AndOp
data EqOp     = Equal | NotEq
data RelOp    = Less | Leq | Greater | Geq
data AddOp    = Plus | Minus
data MulOp    = Times | Slash | Modulo
data UnaryOp  = No | Nega

data EOF      = EOF

data Ident = Ident String deriving (Eq,Ord)
