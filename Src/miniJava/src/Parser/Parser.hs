module Parser.Parser (
    module Parser.Parser
    ) where

infixl 3 +++

newtype Parser a = Parser { parse :: String -> [(a,String)] }

instance Functor Parser where
    fmap f p = Parser ( \cs -> fmap (\(a,cs') -> (f a,cs')) (parse p cs) )

instance Applicative Parser where
    pure f = Parser ( \cs -> [(f,cs)] )
    p <*> q = Parser ( \cs -> do { (f,_) <- parse p cs; parse (fmap f q) cs } )

instance Monad Parser where
    return a = Parser ( \cs -> [(a,cs)] )
    p >>= f = Parser ( \cs -> do { (a,cs') <- parse p cs; parse (f a) cs' } )

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse p cs ++ parse q cs of
                 [] -> []
                 x:xs -> [x]
                 )

zero :: Parser a
zero = Parser ( \cs -> [] )
