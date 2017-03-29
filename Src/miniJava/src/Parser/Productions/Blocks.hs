module Parser.Productions.Blocks (
    block,
    ) where

import Parser.Parser
import Parser.Combinators
import ASTs.Terminals
import ASTs.NonTerminals
import ASTs.ShowTerm
import ASTs.ShowNonTerm
import Parser.Tokens.Keywords
import Parser.Tokens.Operators
import Parser.Productions.Expressions

block :: Parser Block
block = do
    a <- lCurly
    b <- asterisk (two (varDecl) (statement))
    c <- rCurly
    return $ Block a b c

varDecl :: Parser VarDecl
varDecl = do
    a <- varDecl'
    b <- ask (varClause)
    c <- colon
    return $ VarDecl a b c

varDecl' :: Parser VarDecl'
varDecl' = do
    a <- ktype
    b <- ident
    return $ VarDecl' a b

varClause :: Parser VarClause
varClause = do
    a <- assignOp
    b <- expr
    return (VarClause a b)

statement :: Parser Statement
statement = aExpr +++ aReturn +++ aPrint +++ aIf +++ aWhile +++ aColon +++ aBlock
    where aBlock = (block) >>= return . ABlock
          aColon = (colon) >>= return . AColon
          aExpr = do
              a <- expr
              b <- colon
              return $ AExpr a b

aPrint :: Parser Statement
aPrint = do
    a <- ksystem
    b <- dot
    c <- kout
    d <- dot
    e <- kprintln
    f <- lBracket
    g <- expr
    h <- rBracket
    i <- colon
    return $ APrint a b c d e f g h i

aWhile :: Parser Statement
aWhile = do
    a <- kwhile
    b <- lBracket
    c <- expr
    d <- rBracket
    e <- statement
    return $ AWhile a b c d e

aReturn :: Parser Statement
aReturn = do
    a <- kreturn
    b <- ask (expr)
    c <- colon
    return $ AReturn a b c

aIf :: Parser Statement
aIf = do
    a <- kif
    b <- lBracket
    c <- expr
    d <- rBracket
    e <- statement
    f <- ask (elseClause)
    return $ AIf a b c d e f

elseClause :: Parser ElseClause
elseClause = do
    a <- kelse
    b <- statement
    return $ ElseClause a b
