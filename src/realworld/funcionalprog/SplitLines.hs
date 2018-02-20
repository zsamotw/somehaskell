import Data.List as List

splitLines [] = []
splitLines lines =
  let (pre, suf) = break isLineTerminator lines
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r': rest) -> splitLines rest
    ('\n': rest) -> splitLines rest
    _            -> []

isLineTerminator c = c == '\n' || c == '\r'
