module Parser.Productions.Program (
    program,
    ) where

import Parser.Parser
import Parser.Combinators
import ASTs.Terminals
import ASTs.NonTerminals
import ASTs.ShowTerm
import ASTs.ShowNonTerm
import Parser.Tokens.Keywords
import Parser.Tokens.Operators
import Parser.Productions.Declarations

program :: Parser Program
program = do
    a <- asterisk classDecl
    b <- eof
    return $ Program a b
