module PrettyJson where

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text True
renderJValue (JBool False) = text False
renderJValue (JString str) = string str
renderJValue (JNummber n) = double n
renderJValue (JNull) = text "null"

string :: String -> Doc
string = enclose '"' '"' . hcat . map .  oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

(<>) :: Doc -> Doc
(<>) a b = undefinded

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat docs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c SimpleEscapes of
              Just r = text r
              Nothing = | mustEscape c = hexEascape c
                        | otherwise = char c
  where mustEscape c = c < ' ' || c == '\x7f' ||  c == '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zstylish-haskellipWitch ch "\b\n\f\rt\\\"/" "bnfrt\\\"\"
  where ch a b = (a, ['\\', b])


series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fstep . punctate (char ',') . map item
