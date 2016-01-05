module Parser.Combinators (
    sat, sat',
    char,
    string,
    next,
    asterisk, plusSign, ask, two,
    lass, rass,
    --sepBy, sepBy1, chain, chain1,
    token, apply,
    ) where

import Parser.Parser
import qualified Data.Char as Char

nextPos (i,j) '\n' = (i+1,1)
nextPos (i,j) _    = (i,j+1)

item :: Parser Char
item = Parser (\(pos,cs) ->
    case cs of
         c:cs -> let newPos = nextPos pos c
                     newSt  = (newPos,cs)
                     in seq newPos (Right (Right (c,newSt,(pos,[],[])) ) )
         []   -> Left (Left (pos,"EOF",[])) )

sat :: (Char -> Bool) -> Parser Char
sat p = Parser (\(pos,cs) ->
    case cs of
         c:cs | p c ->
             let newPos = nextPos pos c
                 newSt  = (newPos,cs)
             in seq newPos (Right (Right (c,newSt,(pos,[],[])) ) )
         c:cs -> Left (Left (pos,[c],[]))
         []   -> Left (Left (pos,"EOF",[])) )

sat' :: (Char -> Bool) -> Parser String
sat' p = do { x <- sat p; return [x] }

char :: Char -> Parser Char
char c = sat (c==) <?> [c]

string :: String -> Parser String
string cs = Parser (g . parse (h cs))
    where g x = case x of
                     Left  (Left m) -> Left (Left m)
                     Right (Left m) -> Left (Left m)
                     other          -> other
          h "" =  return ""
          h cs0@(c:cs) = char c >> h cs >> return cs0

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

lass :: Parser a -> Parser op -> (a -> b) -> (b -> op -> a -> b) -> Parser b
lass a op single cons = a >>= rest . single
    where rest x = ( do
                   y <- op
                   z <- a
                   rest $ cons x y z
                   ) +++ return x

rass :: Parser a -> Parser op -> (a -> b) -> (a -> op -> b -> b) -> Parser b
rass a op single cons = a >>= rest
    where rest x = ( do
                   y <- op
                   z <- a
                   r <- rest z
                   return $ (cons x y r)
                   ) +++ return (single x)
token :: Parser a -> Parser a
token p = do { xs <- p; white; return xs }

apply :: Parser a -> State -> Eaten a
apply p = parse (white >> p)

white :: Parser String
white = asterisk (space +++ comments) >>= return . concat

space :: Parser String
space = sat' Char.isSpace

comments :: Parser String
comments = (string "/*" >> (next "*/" <?> "*/"))
       +++ (string "//" >> (next "\n" <?> "\n"))

{-
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p q = sepBy1 p q +++ return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p q = do
    x <- p
    xs <- asterisk (q >> p)
    return (x:xs)

chain :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chain p op a = chain1 p op +++ return a

chain1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chain1 p op = do
    x <- p
    rest x where
               rest x = ( do
                        f <- op
                        y <- p
                        rest (f x y) ) +++ return x
-}
