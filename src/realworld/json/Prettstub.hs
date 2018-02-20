module PrettyStub

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc         
           deriving Show

string :: String ->Doc
string s = undefined

text :: String -> Doc
text t = Text t
text "" = Empty

double :: Double -> Doc
double d = text (show d)


fstep :: [Doc] -> Doc
fstep xs = undefined
