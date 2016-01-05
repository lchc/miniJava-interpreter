module Interp.Eval (
    eval,
    ) where

import Interp.Interp
import ASTs.Terminals
import ASTs.NonTerminals
import Data.Bits ((.&.),(.|.))

varDecl :: VarDecl -> Interp Int
varDecl (VarDecl (VarDecl' a b) ca d) = case ca of
                        Nothing -> exti b 0
                        Just (VarClause i j) -> do
                            j' <- expr j
                            exti b j'

expr :: Expr -> Interp Int
expr (AOr a) = orExpr a
expr (Expr a b c) =
    case a of
         (AAnd (AEq (ARel (AAdd (AMul (AUnary (APrimary (ACall (Call (Ref i) Nothing))))))))) -> do
             j <- expr c
             updi i j
             return j

orExpr :: OrExpr -> Interp Int
orExpr (AAnd a) = andExpr a
orExpr (OrExpr a b c) = do
    i <- orExpr a
    j <- andExpr c
    return $ i .|. j

andExpr :: AndExpr -> Interp Int
andExpr (AEq a) = eqExpr a
andExpr (AndExpr a b c) = do
    i <- andExpr a
    j <- eqExpr c
    return $ i .&. j

eqExpr :: EqExpr -> Interp Int
eqExpr (ARel a) = relExpr a
eqExpr (EqExpr a b c) = do
    i <- eqExpr a
    j <- relExpr c
    return (case b of
                 Equal -> if i == j then 1 else 0
                 NotEq -> if i == j then 0 else 1 )

relExpr :: RelExpr -> Interp Int
relExpr (AAdd a) = addExpr a
relExpr (RelExpr a b c) = do
    i <- relExpr a
    j <- addExpr c
    case b of
         Less    -> return $ if i <  j then 1 else 0
         Leq     -> return $ if i <= j then 1 else 0
         Greater -> return $ if i >  j then 1 else 0
         Geq     -> return $ if i >= j then 1 else 0

addExpr :: AddExpr -> Interp Int
addExpr (AMul a) = mulExpr a
addExpr (AddExpr a b c) = do
    i <- addExpr a
    j <- mulExpr c
    case b of
         Plus  -> return $ i + j
         Minus -> return $ i - j

mulExpr :: MulExpr -> Interp Int
mulExpr (AUnary a) = unaryExpr a
mulExpr (MulExpr a b c) = do
    i <- mulExpr a
    j <- unaryExpr c
    case b of
         Times  -> return $ i * j
         Slash  -> return $ i `div` j
         Modulo -> return $ i `mod` j

unaryExpr :: UnaryExpr -> Interp Int
unaryExpr (APrimary a) = primaryExpr a
unaryExpr (UnaryExpr a b) = do
    j <- unaryExpr b
    case a of
         No   -> return $ if j == 0 then 1 else 0
         Nega -> return $ - j

primaryExpr :: PrimaryExpr -> Interp Int
primaryExpr (ACall (Call a Nothing)) = basicExpr a

primaryExpr (ACall (Call (Ref a) (Just (Calling b ca d)))) =
    case ca of
         Nothing -> invoke a (apply $ [])
         Just e  -> do
             xs <- exprs e
             invoke a (apply xs)
    
exprs :: Exprs -> Interp [Int]
exprs (SingleExpr e) = expr e >>= return . (:[])
exprs (Exprs e _ es) = do
    x  <- expr e
    xs <- exprs es
    return $ x:xs

apply :: [Int] -> Func -> Interp Int
apply xs f =  h (return f) xs
    where h f [] = f >>= funci
          h f (x:xs) = do
              f' <- f
              h (funcf f' x) xs

basicExpr :: BasicExpr -> Interp Int
basicExpr e = case e of
                   Int a -> return a
                   Bool True  -> return 1
                   Bool False -> return 0
                   Expression a b c -> expr b
                   Ref a -> vali a

statement :: Statement -> Interp Int
statement (AColon a) = return 0
statement (AExpr a b) = expr a
statement (ABlock a) = closure_ (block a)

statement w@(AWhile a b c d e) = do
    c' <- expr c
    if c' == 0 then return 0 else h (statement e) (expr c)
        where h b c = do
                  b' <- b
                  c' <- c
                  if c' == 0 then return b' else h b c

statement (AIf a b c d e fa) = do
    c' <- expr c
    if not (c' == 0)
       then statement e
       else case fa of
                 Nothing -> return 0
                 Just (ElseClause g h) -> statement h

statement (AReturn a ba c) = case ba of
                                  Nothing -> return_ 0
                                  Just a  -> expr a >>= return_

statement (APrint a b c d e f g h i) = do
    v <- expr g
    out $ show v ++ "\n"

block :: Block -> Interp Int
block (Block a bs c) = ss bs
    where ss [] = return 0
          ss (Left x:xs) = varDecl x   >> ss xs
          ss (Right x:xs)  = statement x >> ss xs

eval :: Program -> Interp Int
eval (Program [ClassDecl a b c d es f] EOF) = closure h
    where h = methods es >> invoke (Ident "main") (apply $ [])
              where methods [] = return 0
                    methods (Member a (Just b) c
                            (Right (MemberClause d (Left e))):xs) =
                            exti d 0 >> methods xs
                    methods (Member a (Just b) c
                            (Right (MemberClause d (Right (Method e fa g h)))):xs) =
                            extf d (func fa h) >> methods xs
                    methods (Member a (Just b) c
                            (Left (Main d e f g h i j k)):xs) =
                            extf (Ident "main") (func Nothing k) >> methods xs
                    methods (x:xs) = methods xs

func :: Maybe Paras -> Block -> Func
func aa b =
    case aa of
         Nothing -> Funci $ block b
         Just (APara (Para _ id)) -> Funcf $ \x -> (exti id x) >> h
             where h = return $ func Nothing b
         Just (Paras (Para _ id) _ p) -> Funcf $ \x -> (exti id x) >> h
             where h = return $ func (Just p) b
{--
invoke :: Ident -> Maybe Exprs -> Interp Int
invoke id ea = valf id >>= closure . apply ea . return

apply :: Maybe Exprs -> Interp Func -> Interp Int
apply Nothing f = f >>= funci
apply (Just (SingleExpr e)) f = do
    e' <- expr e
    f' <- f
    apply Nothing (funcf f' e')
apply (Just (Exprs e _ es)) f = do
    e' <- expr e
    f' <- f
    apply (Just es) (funcf f' e')
--}
--if c' == 0
--then return 0
--else statement e >> statement w
    {--if c' == 0
       then return 0
       else statement e >> statement w--}
