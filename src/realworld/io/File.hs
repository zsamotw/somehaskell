import System.IO
import Data.Char(toUpper)

main :: IO()
main = do
       inh <- openFile "in" ReadMode
       outh <- openFile "out" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
  do iseof <- hIsEOF inh
     if iseof
       then return ()
       else do input <- hGetLine inh
               hPutStrLn outh (map toUpper input)
               mainloop inh outh

mainlazyloop :: Handle -> Handle -> IO()
mainlazyloop inh outh =
  do input <- hGetContents inh
  let res = process input
  hPutStrLn outh res
  where process :: String -> String
        process = map toUpper

maineasly =
  do input <- readFile "in"
  writeFile "out" input


shorter =
  do putStrLn "Your name?" >>
  getLine >>=
  (\input -> PutStrLn "Hallo" ++ input)
