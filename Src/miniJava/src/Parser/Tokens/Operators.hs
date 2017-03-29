module Parser.Tokens.Operators (
    lBracket, rBracket,
    lBrace, rBrace,
    lCurly, rCurly,
    comma, colon, dot,

    assignOp,
    orOp, andOp,
    equal, notEq,
    less, leq, greater, geq,
    plus, minus,
    times, slash, modulo,
    no, nega,
    ) where

import Parser.Parser
import Parser.Combinators
import ASTs.Terminals
import ASTs.NonTerminals
import ASTs.ShowTerm
import ASTs.ShowNonTerm
import qualified Data.Char as Char

lBracket :: Parser LBracket
rBracket :: Parser RBracket
lBrace   :: Parser LBrace
rBrace   :: Parser RBrace
lCurly   :: Parser LCurly
rCurly   :: Parser RCurly
comma    :: Parser Comma
colon    :: Parser Colon
dot      :: Parser Dot

lBracket = token $ string "(" >> return LBracket
rBracket = token $ string ")" >> return RBracket
lBrace   = token $ string "[" >> return LBrace
rBrace   = token $ string "]" >> return RBrace
lCurly   = token $ string "{" >> return LCurly
rCurly   = token $ string "}" >> return RCurly
comma    = token $ string "," >> return Comma
colon    = token $ string ";" >> return Colon
dot      = token $ string "." >> return Dot

assignOp :: Parser AssignOp
orOp     :: Parser OrOp
andOp    :: Parser AndOp
equal    :: Parser EqOp
notEq    :: Parser EqOp
less     :: Parser RelOp
leq      :: Parser RelOp
greater  :: Parser RelOp
geq      :: Parser RelOp
plus     :: Parser AddOp
minus    :: Parser AddOp
times    :: Parser MulOp
slash    :: Parser MulOp
modulo   :: Parser MulOp
no       :: Parser UnaryOp
nega     :: Parser UnaryOp

assignOp = token $ string "="  >> return AssignOp
orOp     = token $ string "||" >> return OrOp
andOp    = token $ string "&&" >> return AndOp
equal    = token $ string "==" >> return Equal
notEq    = token $ string "!=" >> return NotEq
less     = token $ string "<"  >> return Less
leq      = token $ string "<=" >> return Leq
greater  = token $ string ">"  >> return Greater
geq      = token $ string ">=" >> return Geq
plus     = token $ string "+"  >> return Plus
minus    = token $ string "-"  >> return Minus
times    = token $ string "*"  >> return Times
slash    = token $ string "/"  >> return Slash
modulo   = token $ string "%"  >> return Modulo
no       = token $ string "!"  >> return No
nega     = token $ string "-"  >> return Nega
