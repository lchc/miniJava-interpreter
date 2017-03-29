module Parser.Parser (
    module Parser.Parser
    ) where

--import GHC.Base ((<|>),empty,Alternative)

--newtype Parser a = Parser { parse :: (String -> [(a,String)]) }
--newtype Parser a = Parser { parse :: String -> Either (Errs,String) (a,String) }

infixl 1 <?>
infixl 3 +++

type Pos = (Int,Int)

type Msg = (Pos,String,[String])

type State = (Pos,String)

type Reply a = Either Msg (a,State,Msg)

type Eaten a = Either (Reply a) (Reply a)

newtype Parser a = Parser { parse :: State -> Eaten a }

instance Functor Parser where
    fmap f p = Parser (\st ->
        case parse p st of
             Right (Right (a,st,m)) -> Right (Right (f a,st,m)) )

instance Applicative Parser where
    pure a = Parser (\st@(pos,cs) -> Left (Right (a,st,(pos,cs,[])) ) )
    (<*>) p q = Parser (\st ->
        case parse p st of
             Right (Right (f,st,m)) -> parse (fmap f q) st )

instance Monad Parser where
    return a = Parser (\st@(pos,cs) -> Left (Right (a,st,(pos,[],[])) ) )
    p >>= f = Parser (\st ->
        case parse p st of
             Left r ->
                 case r of
                      Right (a,st,m) -> parse (f a) st
                      Left  m        -> Left (Left m)
             Right r ->
                 Right
                 (case r of
                        Right (a,st,m) ->
                            case parse (f a) st of
                                 Right r' -> r'
                                 Left  r' -> r'
                        Left m -> Left m
                  ) )

return_ :: a -> Parser a
return_ a = Parser (\st@(pos,cs) -> Left (Left (pos,[],[])) )

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\st ->
    case parse p st of
         Left (Left m) ->
             case parse q st of
                  Left (Left m') ->
                      megErr m m'
                  Left (Right (a,inp,m')) ->
                      megOk a inp m m'
                  right ->
                      right
         Left (Right (a,inp,m)) ->
             case parse q st of
                  Left (Left m') ->
                      megOk a inp m m'
                  Left (Right (_,_,m')) ->
                      megOk a inp m m'
                  right ->
                      right
         other -> other
         {--e0@(Right (Left m)) ->
             case parse q st of
                  e@(Right (Right m')) -> e
                  e@(Left  (Right m')) -> e
                  other                -> e0
         other -> other--} )
    where megOk a inp m m' = Left (Right (a,inp,meg m m'))
          megErr m m' = Left (Left (meg m m'))
          meg (pos,inp,exp1) (_,_,exp2) = (pos,inp,exp1 ++ exp2)

(<?>) :: Parser a -> String -> Parser a
p <?> exp = Parser (\st ->
    case parse p st of
         Left (Left m) -> Left (Left (expect m exp))
         Left (Right (a,st,m)) -> Left (Right (a,st,(expect m exp)) )
         right -> right )
    where expect (pos,inp,_) exp = (pos,inp,[exp])

{-
(+++) p q = Parser $ (\cs -> case parse (p <|> q) cs of
                                  [] -> []
                                  (x:xs) -> [x])
                                  -}
    --p >>= f = Parser (\cs -> concat [parse (f r0) cs0 | (r0,cs0) <- parse p cs])
{-
instance Alternative Parser where
    empty = Parser $ const []
    (<|>) p q = Parser $ (\cs -> parse p cs ++ parse q cs)
--}
