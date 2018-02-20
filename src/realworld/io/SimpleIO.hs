main = do
       putStrLn "your name:"
       name <- getLine
       let outres = "your name" ++ (welcome name)
       putStrLn outres

welcome :: String -> String
welcome name = "hello "  ++ name ++ "!!!letters: " ++ chars
  where chars = show (length name)
