module ASTs.ShowNonTerm (
    module ASTs.ShowNonTerm
    ) where

import ASTs.NonTerminals
import ASTs.ShowTerm

tabstop :: String
tabstop = "    "

indent :: String -> String
indent = concat . fmap (\xs -> tabstop ++ xs ++ "\n") . lines 

tab :: (Show a) => a -> String
tab = indent . show

ss :: (Show a) => [a] -> String
ss xs = concat . fmap tab $ xs

sa :: (Show a) => Maybe a -> String
sa Nothing = ""
sa (Just x) = tab x

se :: (Show a, Show b) => Either a b -> String
se (Left x)  = tab x
se (Right y) = tab y

sf :: (Show a, Show b) => [Either a b] -> String
sf xs = concat . fmap se $ xs

instance Show Program where
    show (Program as b) = x ++ (ss as) ++ (tab b) ++ ")"
        where x = "(Program\n"

instance Show ClassDecl where
    show (ClassDecl a b ca d es f) = x ++ a' ++ b' ++ ca' ++ d' ++ es' ++ f' ++ ")"
        where x   = "(ClassDecl\n"
              a'  = tab a
              b'  = tab b
              ca' = sa ca
              d'  = tab d
              es' = ss es
              f'  = tab f

instance Show Extending where
    show (Extending a b) = x ++ (tab a) ++ (tab b) ++ ")"
        where x   = "(Extending\n"

instance Show Member where
    show (Member a ba c (Right (MemberClause d (Left e)))) =
        x ++ tab a ++ sa ba ++ tab c ++ tab d ++ tab e ++ ")"
            where x = "(Field\n"
    show (Member a ba c (Right (MemberClause d (Right (Method e fa g h))))) =
        x ++ tab a ++ sa ba ++ tab c ++ tab d ++ tab e ++ sa fa ++ tab g ++ tab h ++ ")"
            where x = "(Method\n"
    show (Member a ba c (Left (Main d e f g h i j k))) =
        x ++ tab a ++ sa ba ++ tab c ++ tab d ++ tab e ++ tab f ++ tab g ++ tab h ++ tab i ++ tab j
        ++ tab k ++ ")"
            where x = "(MainMethod\n"

instance Show Main where
    show (Main a b c d e f g h) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ (tab d) ++ (tab e)
                             ++ (tab f) ++ (tab g) ++ (tab h) ++ ")"
        where x = "(Main\n"

instance Show MemberClause where
    show (MemberClause a be) = x ++ (tab a) ++ (se be) ++ ")"
        where x = "(MemberClause\n"

instance Show Method where
    show (Method a ba c d) = x ++ (tab a) ++ (sa ba) ++ (tab c) ++ (tab d) ++ ")"
        where x = "(Method\n"

instance Show Paras where
    show (APara a) = show a
    show (Paras a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(Paras \n"

instance Show Para where
    show (Para a b) = x ++ (show a) ++ " " ++ (show b) ++ ")"
        where x = "(Para "

instance Show Block where
    show (Block a bf c) = x ++ (tab a) ++ (sf bf) ++ (tab c) ++ ")"
        where x = "(Block\n"

instance Show VarDecl where
    show (VarDecl (VarDecl' a b) ca d) = x ++ (tab a) ++ (tab b) ++ (sa ca) ++ (tab d) ++ ")"
        where x = "(VarDecl\n"

instance Show VarClause where
    show (VarClause a b) = x ++ (tab a) ++ (tab b) ++ ")"
        where x = "(VarClause\n"

instance Show Statement where
    show (ABlock a) = show a
    show (AColon a) = show a
    show (APrint a b c d e f g h i) =
        x ++ a' ++ b' ++ c' ++ d' ++ e' ++ f' ++ g' ++ h' ++ i' ++ ")"
        where x   = "(PrintStatement\n"
              a'  = tab a
              b'  = tab b
              c'  = tab c
              d'  = tab d
              e'  = tab e
              f'  = tab f
              g'  = tab g
              h'  = tab h
              i'  = tab i
    show (AExpr a b) = x ++ (tab a) ++ (tab b) ++ ")"
        where x   = "(ExprStatement\n"
    show (AWhile a b c d e) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ (tab d) ++ (tab e) ++ ")"
        where x   = "(WhileStatement\n"
    show (AReturn a ba c) = x ++ (tab a) ++ (sa ba) ++ (tab c) ++ ")"
        where x   = "(ReturnStatement\n"
    show (AIf a b c d e fa) = x ++ a' ++ b' ++ c' ++ d' ++ e' ++ fa' ++ ")"
        where x   = "(IfStatement\n"
              a'  = tab a
              b'  = tab b
              c'  = tab c
              d'  = tab d
              e'  = tab e
              fa' = sa fa

instance Show ElseClause where
    show (ElseClause a b) = x ++ (tab a) ++ (tab b) ++ ")"
        where x = "(ElseClause\n"

instance Show Expr where
    show (AOr a) = show a
    show (Expr a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(Expr\n"

instance Show OrExpr where
    show (AAnd a) = show a
    show (OrExpr a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(OrExpr\n"

instance Show AndExpr where
    show (AEq a) = show a
    show (AndExpr a b c ) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(AndExpr\n"

instance Show EqExpr where
    show (ARel a) = show a
    show (EqExpr a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(EqExpr\n"

instance Show RelExpr where
    show (AAdd a) = show a
    show (RelExpr a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(RelationExpr\n"

instance Show AddExpr where
    show (AMul a) = show a
    show (AddExpr a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(AddExpr\n"

instance Show MulExpr where
    show (AUnary a) = show a
    show (MulExpr a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(MulExpr\n"

instance Show UnaryExpr where
    show (APrimary a) = show a
    show (UnaryExpr a b) = x ++ (tab a) ++ (tab b) ++ ")"
        where x = "(UnaryExpr\n"

instance Show PrimaryExpr where
    show (ACall a) = show a
    show (Calls a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where  x = "(PrimaryExpr\n"

instance Show Call where
    show (Call a Nothing) = show a
    show (Call a ba) = x ++ (tab a) ++ (sa ba) ++ ")"
        where x = "(Call\n"

instance Show Calling where
    show (Calling a ba c) = x ++ (tab a) ++ (sa ba) ++ (tab c) ++ ")"
        where x = "(Calling\n"

instance Show BasicExpr where
    show (Bool True) = "(key true)"
    show (Bool False) = "(key false)"
    show (Int a)  = "(int " ++ show a ++ ")"
    show (Nul)    = "(key null)"
    show (This)   = "(key this)"
    show (NewExpr a b c d) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ (tab d) ++  ")"
        where x   = "(NewExpr\n"
    show (Expression a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x   = "(BasicExpr\n"
    show (Ref a)  = show a

instance Show Exprs where
    show (SingleExpr a) = show a
    show (Exprs a b c) = x ++ (tab a) ++ (tab b) ++ (tab c) ++ ")"
        where x = "(ExprList\n"

instance Show Type where
    show IntType  = "(type int)"
    show BoolType = "(type bool)"
    show VoidType = "(type void)"
    show (ClassType a) = show a

