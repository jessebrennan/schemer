module AST
   ( LispVal(Atom, List, DottedList, Number, String, Bool, Character),
     unwordsList
   ) where


------------------------------------------------------------------------
-- AST
------------------------------------------------------------------------
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             {-| Float Float-}

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Number contents) = show contents
showVal (Character contents) = "#\\" ++ [contents]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . "
                          ++ showVal tail ++ ")"

instance Show LispVal where show = showVal
