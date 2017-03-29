module Parser.Combinators (
    sat, sat',
    char,
    string,
    next,
    asterisk, plusSign, ask, two,
    lass, rass,
    token, apply,
    ) where

import Parser.Parser
import qualified Data.Char as Char

item :: Parser Char
item = Parser f
    where f [] = []
          f (c:cs) = [(c,cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- item; if p c then return c else zero }

sat' :: (Char -> Bool) -> Parser String
sat' p = do { x <- sat p; return [x] }

char :: Char -> Parser Char
char c = sat (c==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = char c >> string cs >> return (c:cs)

next :: String -> Parser String
next cs = string cs +++ (item >> next cs)

asterisk :: Parser a -> Parser [a]
asterisk p = plusSign p +++ return []

plusSign :: Parser a -> Parser [a]
plusSign p = do { x <- p; xs <- asterisk p; return (x:xs) }

ask :: Parser a -> Parser (Maybe a)
ask p = (p >>= return . Just) +++ return Nothing

two :: Parser a -> Parser b -> Parser (Either a b)
two p q = (p >>= return . Left) +++ (q >>= return . Right)

-- ( (a op a) op a ) op a
lass :: Parser a -> Parser op -> (a -> b) -> (b -> op -> a -> b) -> Parser b
lass a op single cons = a >>= rest . single
    where rest x = ( do
                   y <- op
                   z <- a
                   rest $ cons x y z
                   ) +++ return x

-- a op ( a op (a op a) )
rass :: Parser a -> Parser op -> (a -> b) -> (a -> op -> b -> b) -> Parser b
rass a op single cons = a >>= rest
    where rest x = ( do
                   y <- op
                   z <- a
                   r <- rest z
                   return $ (cons x y r)
                   ) +++ return (single x)

token :: Parser a -> Parser a
token p = do { a <- p; white; return a }

apply :: Parser a -> String -> [(a,String)]
apply p = parse (white >> p)

white :: Parser String
white = asterisk (space +++ comments) >>= return . concat

space :: Parser String
space = sat' Char.isSpace

comments :: Parser String
comments = (string "/*" >> next "*/") +++ (string "//" >> next "\n")
