module ASTs.NonTerminals (
    module ASTs.NonTerminals
    ) where

import ASTs.Terminals

data Program = Program [ClassDecl] EOF

data ClassDecl = ClassDecl Class Ident (Maybe Extending) LCurly [Member] RCurly

data Extending = Extending Extends Ident

data Member = Member Public (Maybe Static) Type (Either Main MemberClause)

data Main = Main KMain LBracket KString LBrace RBrace Ident RBracket Block

data MemberClause = MemberClause Ident (Either Colon Method)

data Method = Method LBracket (Maybe Paras) RBracket Block

data Paras = APara Para
           | Paras Para Comma Paras

data Para = Para Type Ident

data Block = Block LCurly [Either VarDecl Statement] RCurly

data VarDecl = VarDecl VarDecl' (Maybe VarClause) Colon

data VarDecl' = VarDecl' Type Ident

data VarClause = VarClause AssignOp Expr

data Statement = ABlock  Block
               | AColon  Colon
               | APrint  System Dot Out Dot Println LBracket Expr RBracket Colon
               | AExpr   Expr Colon
               | AWhile  While LBracket Expr RBracket Statement
               | AReturn Return (Maybe Expr) Colon
               | AIf     If LBracket Expr RBracket Statement (Maybe ElseClause)

data ElseClause = ElseClause Else Statement

data Expr      = AOr OrExpr           | Expr OrExpr AssignOp Expr
data OrExpr    = AAnd AndExpr         | OrExpr OrExpr OrOp AndExpr
data AndExpr   = AEq EqExpr           | AndExpr AndExpr AndOp EqExpr
data EqExpr    = ARel RelExpr         | EqExpr EqExpr EqOp RelExpr
data RelExpr   = AAdd AddExpr         | RelExpr RelExpr RelOp AddExpr
data AddExpr   = AMul MulExpr         | AddExpr AddExpr AddOp MulExpr
data MulExpr   = AUnary UnaryExpr     | MulExpr MulExpr MulOp UnaryExpr
data UnaryExpr = APrimary PrimaryExpr | UnaryExpr UnaryOp UnaryExpr

data PrimaryExpr = ACall Call
                 | Calls PrimaryExpr Dot Call

data Call = Call BasicExpr (Maybe Calling)

data Calling = Calling LBracket (Maybe Exprs) RBracket

data BasicExpr = Bool Bool | Int Int | Nul | This
           | NewExpr New Ident LBracket RBracket
           | Expression LBracket Expr RBracket
           | Ref Ident

data Exprs = SingleExpr Expr
           | Exprs Expr Comma Exprs

data Type = IntType | BoolType | VoidType | ClassType Ident
