import Data.List
--import Json
  
renderJson :: JValue -> String

renderJson (JString s) = show s
renderJson (JNumber n) = show n
renderJson (JBool b) = show b
renderJson (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate "," (map renderPairs ps)
        renderPairs (k,v) = show k ++ ":" renderJson v
renderJson (JArray a) = "[" ++ renderArray ++ "]"
  where renderArray [] = ""
        renderArray a = intercalate "," (map renderJson a)

printJson :: JValue -> IO()
printJson json = putStrLn(renderJson json)
