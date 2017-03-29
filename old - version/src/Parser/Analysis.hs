module Parser.Analysis (
    analyse,
    ) where

import Parser.Parser
import Parser.Combinators
import Parser.Productions.Program
import ASTs.NonTerminals
import Data.List

err :: Msg -> String
err ((i,j),inp,exp) = "|" ++ show i ++ "| " ++ show j ++ ": unexpected " ++ show inp ++ "\n" ++ h exp
    where h [] = ""
          h [e] = "\texpected " ++ show e ++ "\n"
          h (e:es) = "\texpecting " ++ concat (intersperse ", " es) ++ " or " ++ e ++ "\n"

analyse :: String -> Either String Program
analyse a = case apply program ((1,1),a) of
                 Right (Right (a,_,_)) -> Right a
                 Left  (Right (a,_,_)) -> Right a
                 Right (Left  m)       -> Left (err m)
                 Left  (Left  m)       -> Left (err m)
