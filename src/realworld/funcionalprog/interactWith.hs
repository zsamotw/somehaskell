import Data.Char
import System.Environment (getArgs)

splitLines [] = []
splitLines lines =
  let (pre, suf) = break isLineTerminator lines
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r': rest) -> splitLines rest
    ('\n': rest) -> splitLines rest
    _            -> []

isLineTerminator c = c == '\n' || c == '\r'

fixLines :: String -> String
fixLines lines = unlines (splitLines)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
              [inputF, outputF] -> interactWith myFunction inputF outputF
              _ -> putStrLn "error Not enought args"
          myFunction = fixLines
              
