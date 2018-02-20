
punctate :: Doc -> [Doc] -> Doc
punctate p [] = p
punctate p [d] = [d]
punctate p [d:ds] = (d <> p) : punctate p ds
