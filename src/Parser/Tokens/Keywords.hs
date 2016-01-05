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
ident = Parser (g . parse h) <?> "Ident"
    where g x = case x of
                     Left  (Left m) -> Left (Left m)
                     Right (Left m) -> Left (Left m)
                     other          -> other
          h = do
              c <- char '_' +++ sat Char.isAlpha
              cs <- asterisk $ char '_' +++ sat Char.isAlphaNum
              if Set.member (c:cs) reserved
                 then return_ . Ident $ (c:cs)
                 else token . return  . Ident $ (c:cs)

int  :: Parser BasicExpr
bool :: Parser BasicExpr
nul  :: Parser BasicExpr
this :: Parser BasicExpr

int = token $ plusSign (sat Char.isDigit) >>= return . Int . read <?> "int"

bool = token (true +++ false)
    where true  = string "true"  >> return (Bool True) <?> "true"
          false = string "false" >> return (Bool False) <?> "false"

nul = token (string "null" >> return Nul) <?> "null"

this = token (string "this" >> return This) <?> "this"


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

kclass   = token $ string "class"   >> return Class <?> "class"
kextends = token $ string "extends" >> return Extends <?> "extends"
kpublic  = token $ string "public"  >> return Public <?> "public"
kstatic  = token $ string "static"  >> return Static <?> "static"
kwhile   = token $ string "while"   >> return While <?> "while"
kif      = token $ string "if"      >> return If <?> "if"
kelse    = token $ string "else"    >> return Else <?> "else"
ksystem  = token $ string "System"  >> return System <?> "System"
kout     = token $ string "out"     >> return Out <?> "out"
kprintln = token $ string "println" >> return Println <?> "println"
kreturn  = token $ string "return"  >> return Return <?> "return"
knew     = token $ string "new"     >> return New <?> "new"
kmain    = token $ string "main"    >> return KMain <?> "main"
kstring  = token $ string "String"  >> return KString <?> "String"
kvoid    = token $ string "void"    >> return Void <?> "void"

ktype :: Parser Type
ktype = intType +++ boolType +++ voidType +++ classType <?> "Type"

intType   :: Parser Type
boolType  :: Parser Type
voidType  :: Parser Type
classType :: Parser Type

intType   = token $ string "int" >> return IntType <?> "int"
boolType  = token $ string "boolean" >> return BoolType <?> "boolean"
voidType  = token $ string "void" >> return VoidType <?> "void"
classType = token $ ident  >>= return . ClassType

eof :: Parser EOF
eof = Parser (\st@(pos,cs) ->
    case cs of
         c:cs -> Left (Left (pos,[c],["EOF"]))
         []   -> Left (Right (EOF,st,(pos,[],[])) ) )
