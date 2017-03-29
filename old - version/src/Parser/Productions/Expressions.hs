module Parser.Productions.Expressions (
    expr, 
    ) where

import Parser.Parser
import Parser.Combinators
import ASTs.Terminals
import ASTs.NonTerminals
import ASTs.ShowTerm
import ASTs.ShowNonTerm
import Parser.Tokens.Keywords
import Parser.Tokens.Operators

expr      :: Parser Expr
orExpr    :: Parser OrExpr
andExpr   :: Parser AndExpr
eqExpr    :: Parser EqExpr
relExpr   :: Parser RelExpr
addExpr   :: Parser AddExpr
mulExpr   :: Parser MulExpr

expr    = rass orExpr    assignOp AOr    Expr <?> "Expr"
orExpr  = lass andExpr   orOp     AAnd   OrExpr <?> "OrExpr"
andExpr = lass eqExpr    andOp    AEq    AndExpr <?> "AndExpr"
eqExpr  = lass relExpr   eqOp     ARel   EqExpr <?>  "EqExpr"
relExpr = lass addExpr   relOp    AAdd   RelExpr <?> "RelExpr"
addExpr = lass mulExpr   addOp    AMul   AddExpr <?> "AddExpr"
mulExpr = lass unaryExpr mulOp    AUnary MulExpr <?> "MulExpr"

unaryExpr :: Parser UnaryExpr
unaryExpr = ( do
            a <- unaryOp
            b <- unaryExpr
            return $ UnaryExpr a b ) +++ (primaryExpr >>= return . APrimary)
            <?> "UnaryExpr"

eqOp :: Parser EqOp
relOp :: Parser RelOp
addOp :: Parser AddOp
mulOp :: Parser MulOp
unaryOp :: Parser UnaryOp

eqOp = equal +++ notEq
relOp = leq +++ geq +++ less +++ greater
addOp = plus +++ minus
mulOp = times +++ slash +++ modulo
unaryOp = no +++ nega

primaryExpr :: Parser PrimaryExpr
primaryExpr = lass call dot ACall Calls <?> "PrimaryExpr"

call :: Parser Call
call = (do
    a <- basicExpr
    b <- ask calling
    return $ Call a b )
    <?> "Call"

calling :: Parser Calling
calling = (do
    a <- lBracket
    b <- ask exprs
    c <- rBracket
    return $ Calling a b c )
    <?> "Calling"

basicExpr :: Parser BasicExpr
basicExpr = ref +++ bool +++ int +++ nul +++ this +++ newExpr +++ expression <?> "BasicExpr"

newExpr :: Parser BasicExpr
newExpr = (do
    a <- knew
    b <- ident
    c <- lBracket
    d <- rBracket
    return $ NewExpr a b c d)

expression :: Parser BasicExpr
expression = (do
    a <- lBracket
    b <- expr
    c <- rBracket
    return $ Expression a b c)

exprs :: Parser Exprs
exprs = rass expr comma SingleExpr Exprs <?> "Exprs"

ref :: Parser BasicExpr
ref = ident >>= return . Ref
