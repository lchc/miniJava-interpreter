module Parser.Productions.Declarations (
    classDecl,
    ) where

import Parser.Parser
import Parser.Combinators
import ASTs.Terminals
import ASTs.NonTerminals
import ASTs.ShowTerm
import ASTs.ShowNonTerm
import Parser.Tokens.Keywords
import Parser.Tokens.Operators
import Parser.Productions.Blocks

classDecl :: Parser ClassDecl
classDecl = (do
    a <- kclass
    b <- ident
    c <- ask (extending)
    d <- lCurly
    e <- asterisk (member)
    f <- rCurly
    return $ ClassDecl a b c d e f )
    <?> "ClassDecl"

extending :: Parser Extending
extending = (do
    a <- kextends
    b <- ident
    return $ Extending a b )
    <?> "Extending"

member :: Parser Member
member = (do
    a  <- kpublic
    ba <- ask (kstatic)
    c  <- ktype
    d  <- two main memberClause
    return $ Member a ba c d )
    <?> "Member"

main :: Parser Main
main= (do
    a <- kmain
    b <- lBracket
    c <- kstring
    d <- lBrace
    e <- rBrace
    f <- ident
    g <- rBracket
    h <- block
    return $ Main a b c d e f g h )

memberClause :: Parser MemberClause
memberClause = (do
    a <- ident
    b <- two colon method
    return $ MemberClause a b )

method :: Parser Method
method = (do
    a  <- lBracket
    ba <- ask (paras)
    c  <- rBracket
    d  <- block
    return $ Method a ba c d )

paras :: Parser Paras
paras = rass para comma APara Paras <?> "Paras"

para :: Parser Para
para = (do
    a <- ktype
    b <- ident
    return (Para a b))
    <?> "Para"
