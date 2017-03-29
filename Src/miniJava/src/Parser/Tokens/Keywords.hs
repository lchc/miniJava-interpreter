module Parser.Tokens.Keywords (
    ident,
    int, bool,
    nul,
    this,

    kclass, kextends,
    kpublic, kstatic,
    kwhile, kif, kelse,
    ksystem, kout, kprintln,
    kreturn,
    knew,
    kmain,
    kstring,
    kvoid,

    intType, boolType, voidType, classType, ktype,

    eof,
    ) where

import Parser.Parser
import Parser.Combinators
import ASTs.Terminals
import ASTs.NonTerminals
import ASTs.ShowTerm
import ASTs.ShowNonTerm
import qualified Data.Char as Char
import qualified Data.Set  as Set

reserved :: Set.Set String
reserved = Set.fromList ["return","if","while","new","System","this","int","boolean","void"
                         ,"null","true","false"]

ident :: Parser Ident
ident = do
    c  <- char '_' +++ sat Char.isAlpha
    cs <- asterisk $ char '_' +++ sat Char.isAlphaNum
    if Set.member (c:cs) reserved
       then zero
       else token . return . Ident $ c:cs

int  :: Parser BasicExpr
bool :: Parser BasicExpr
nul  :: Parser BasicExpr
this :: Parser BasicExpr

int = token $ plusSign (sat Char.isDigit) >>= return . Int . read

bool = token (true +++ false)
    where true  = string "true"  >> return (Bool True)
          false = string "false" >> return (Bool False)

nul = token (string "null" >> return Nul)

this = token (string "this" >> return This)


kclass   :: Parser Class
kextends :: Parser Extends
kpublic  :: Parser Public
kstatic  :: Parser Static
kwhile   :: Parser While
kif      :: Parser If
kelse    :: Parser Else
ksystem  :: Parser System
kout     :: Parser Out
kprintln :: Parser Println
kreturn  :: Parser Return
knew     :: Parser New
kmain    :: Parser KMain
kvoid    :: Parser Void

kclass   = token $ string "class"   >> return Class
kextends = token $ string "extends" >> return Extends
kpublic  = token $ string "public"  >> return Public
kstatic  = token $ string "static"  >> return Static
kwhile   = token $ string "while"   >> return While
kif      = token $ string "if"      >> return If
kelse    = token $ string "else"    >> return Else
ksystem  = token $ string "System"  >> return System
kout     = token $ string "out"     >> return Out
kprintln = token $ string "println" >> return Println
kreturn  = token $ string "return"  >> return Return
knew     = token $ string "new"     >> return New
kmain    = token $ string "main"    >> return KMain
kstring  = token $ string "String"  >> return KString
kvoid    = token $ string "void"    >> return Void

ktype :: Parser Type
ktype = intType +++ boolType +++ voidType +++ classType

intType   :: Parser Type
boolType  :: Parser Type
voidType  :: Parser Type
classType :: Parser Type

intType   = token $ string "int" >> return IntType
boolType  = token $ string "boolean" >> return BoolType
voidType  = token $ string "void" >> return VoidType
classType = token $ ident  >>= return . ClassType

eof :: Parser EOF
eof = Parser f
    where f "" = [(EOF,[])]
          f _  = []
