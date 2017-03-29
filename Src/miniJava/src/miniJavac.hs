import Data.List (isSuffixOf, intersperse)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.IO (readFile)
import Parser.Combinators (apply)
import Parser.Productions.Program  (program)

compile :: String -> String
compile code = case apply program code of
                    (a,cs) : _ -> show a ++ "\n" ++ cs
                    _ -> "parse error"

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
                        else readFile a >>= putStr . compile
