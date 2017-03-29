module Interp.Interp (
    module Interp.Interp,
    ) where

import ASTs.Terminals
import qualified Data.Map as Map

newtype Env = Env (Map.Map Ident Int, Map.Map Ident Func)

newtype Interp a = Interp { interp :: [Env] -> String -> Either (a,[Env],String) (Int,[Env],String) }

instance Functor Interp where
    fmap f i = Interp ( \es0 cs0 -> case interp i es0 cs0 of
                                         Left (a,es,cs) -> Left (f a,es,cs)  )

instance Applicative Interp where
    pure a = Interp ( \es0 cs0 -> Left (a,es0,cs0) )
    i <*> j = Interp ( \es0 cs0 -> case interp i es0 cs0 of
                                        Left (f,_,_) -> interp (fmap f j) es0 cs0 )

instance Monad Interp where
    return a = Interp ( \es0 cs0 -> Left (a,es0,cs0) )
    i >>= h = Interp ( \es0 cs0 -> case interp i es0 cs0 of
                                       Left  (a,es,cs) -> interp (h a) es cs
                                       Right (a,es,cs) -> Right  (a,es,cs)
                     )

data Var = Vari { vari :: Int }
         | Varf { varf :: Func }

data Func = Funci { funci :: Interp Int }
          | Funcf { funcf :: Int -> Interp Func }

env0 = [] :: [Env]
env1 = Env (Map.empty,Map.empty) :: Env

return_ :: Int -> Interp a
return_ a = Interp ( \es cs -> Right (a,es,cs) )

closure :: Interp Int -> Interp Int
closure i = Interp (\es0 cs0 -> case interp i (env1:es0) cs0 of
                                   Right (a,e:es,cs) -> Left (a,es,cs)
                                   Left  (a,e:es,cs) -> Left (a,es,cs)
                  )

closure_ :: Interp Int -> Interp Int
closure_ i = Interp (\es0 cs0 -> case interp i (env1:es0) cs0 of
                                   Right (a,e:es,cs) -> Right (a,es,cs)
                                   Left  (a,e:es,cs) -> Left  (a,es,cs)
                    )

exti :: Ident -> Int -> Interp Int
exti k v = Interp (\(Env (e,f):es) cs -> Left (0, Env (Map.insert k v e,f):es, cs) )

extf :: Ident -> Func -> Interp Int
extf k v = Interp (\(Env (e,f):es) cs -> Left (0, Env (e,Map.insert k v f):es, cs) )

out :: String -> Interp Int
out cs = Interp (\es cs0 -> Left (0, es, cs0 ++ cs) )

vali :: Ident -> Interp Int
vali a = Interp (\es cs -> Left (h a es, es, cs) )
    where h a (Env (e,f):es) =
              case Map.lookup a e of
                   Just a  -> a
                   Nothing -> h a es
          h (Ident a) _ = error a

updi :: Ident -> Int -> Interp Int
updi k v = Interp h
    where h es0@(env@(Env (e,f)):es) cs =
              case Map.lookup k e of
                   Just a  -> Left (0, Env (Map.insert k v e,f):es, cs)
                   Nothing -> case h es cs of
                                   Left (a,es,cs) -> Left (a,env:es,cs)

invoke :: Ident -> (Func -> Interp Int) -> Interp Int
invoke id apply = Interp h
    where h es0@(env@(Env (e,f)):es) cs =
              case Map.lookup id f of
                   Just a  -> interp (closure . apply $ a) es0 cs
                   Nothing -> case h es cs of
                                   Left (a,es,cs) -> Left (a,env:es,cs)
{--
vali :: Ident -> Interp Int
vali k = Interp h
    where h es0@(env@(Env (e,f)):es) cs =
              case Map.lookup k e of
                   Just a  -> Left (a, es0, cs)
                   Nothing -> case h es cs of
                                   Left (a,es,cs) -> Left (a,env:es,cs)

--}
{--
valf :: Ident -> Interp Func
valf a = Interp (\es cs -> Left (h a es, es, cs) )
    where h a (Env (e,f):es) =
              case Map.lookup a f of
                   Just a  -> a
                   Nothing -> h a es
--}
