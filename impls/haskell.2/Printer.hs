module Printer (prStr) where
import Types ( MalType(..), decodeKey )
import Data.Map.Strict (Map, toList, size)

prStr :: MalType ->Bool -> String
prStr (MalNumber num) _     = show num
prStr (MalSymbol sym) _     = sym
prStr (MalString str) False = str
prStr (MalString str) True  =  prettyStr str
prStr (MalKeyword kw) _     = ':':kw
prStr (MalBool bool)  _     = if bool then "true" else "false"
prStr MalNil          _     = "nil"
prStr (MalList list)  x     = printSeq list x ("(", ")")
prStr (MalVector vec) x     = printSeq vec x ("[", "]")
prStr (MalMap map)    x     = printMap map x
prStr (MalFn f)       _     = "#<function>"

prettyStr :: String -> String
prettyStr x = "\""++ foldl (\acc x -> acc ++ escChar x) "" x ++ "\"" 
    where 
        escChar :: Char -> String
        escChar '\n' = "\\n"
        escChar '\\' = "\\\\"
        escChar '"' = "\\\""
        escChar c    = [c]

showSeq :: [MalType] -> Bool -> String
showSeq x rdbly =  foldl (\acc x -> acc ++ prStr x rdbly ++ " ") "" x

printSeq :: [MalType] -> Bool -> (String, String) -> String
printSeq x _ (l,r) | null x = l++r
printSeq x rdbly (l,r) = ((l++) . (++r) . init) (showSeq x rdbly)

printMap :: Map String MalType -> Bool -> String
printMap map _ | null map = "{}"
printMap map rdbly = (("{"++) . (++"}") . init) (printTupleSeq (toList map) rdbly)

printTupleSeq :: [(String,MalType)] -> Bool -> String
printTupleSeq ((k,v):xs) rdbly = prStr (decodeKey k) rdbly ++ (" "++prStr v rdbly++" ") ++ printTupleSeq xs rdbly
printTupleSeq [] _ = ""
