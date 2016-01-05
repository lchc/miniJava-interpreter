import Data.List (isSuffixOf, intersperse)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (readFile)
import Parser.Analysis
import Interp.Execution

run :: String -> String -> String
run f a = case analyse a of
                 Left cs -> f ++ cs
                 Right x -> execute x

main :: IO ()
main = do
    args <- getArgs
    case args of
         []    -> putStrLn "miniJava: no input files"
         (a:_) -> do
             exist <- doesFileExist a
             if not exist
                then putStrLn $ "miniJava: " ++ a ++ ": no such file"
                else if not $ isSuffixOf ".java" a
                        then putStrLn "miniJava: filetype unsupported"
                        else readFile a >>= putStr . run a
