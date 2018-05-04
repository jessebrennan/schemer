module Lib
    ( readExpr,
      eval
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

{-Might have bug with evaluating strings-}

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             {-| Float Float-}

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Number contents) = show contents
showVal (Character contents) = "#" ++ [contents]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . "
                          ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- decided to always just and the operation if done on a list because
-- not sure if this behavior is even expected and this seemed like the
-- safest option
boolOp :: (LispVal -> Bool) -> [LispVal] -> LispVal
boolOp op params = Bool . and . (map op) $ params

isSym :: LispVal -> Bool
isSym (Atom _) = True
isSym _        = False

isStr :: LispVal -> Bool
isStr (String _) = True
isStr _          = False

isNum :: LispVal -> Bool
isNum (Number _) = True
isNum _          = False

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _        = False

isChar :: LispVal -> Bool
isChar (Character _) = True
isChar _             = False

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", boolOp isSym),
              ("string?", boolOp isStr),
              ("number?", boolOp isNum),
              ("bool?", boolOp isBool),
              ("char?", boolOp isChar)]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args

-----------------------------------------------------------------------
-- parser stuff
-----------------------------------------------------------------------

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
   char '"'
   x <- many $ noneOf "\"" <|> (char '\\' >> char '\"')
   char '"'
   return $ String x

parseAtom :: Parser LispVal
parseAtom = do
   first <- letter <|> symbol
   rest <- many (letter <|> digit <|> symbol)
   let atom = first:rest
   return $ case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _    -> Atom atom

-- give a prefix string and a function (like readDec from Numeric)
-- to make the parser object
parseNumPrefix :: Char -> (String -> [(Integer, String)]) -> Parser LispVal
parseNumPrefix letter reader = do
      char letter
      num <- many1 (digit <|> oneOf "abcdefABCDF")
      let [(val, _)] = reader num
      return . Number $ val

parseDec :: Parser LispVal
parseDec = parseNumPrefix 'd' readDec

parseOct :: Parser LispVal
parseOct = parseNumPrefix 'o' readOct

parseHex :: Parser LispVal
parseHex = parseNumPrefix 'x' readHex

parseNumber :: Parser LispVal
parseNumber = ((many1 digit) >>= (return . Number . read))
   <|> try (char '#' >> (parseDec <|> parseOct <|> parseHex))

-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber = do
--    num <- many1 digit
--    return (Number . read $ num)

parseChar :: Parser LispVal
parseChar = try charPrefix >> ((anyChar >>= (return . Character))
                       <|> (string "space"   >> (return . Character $ ' '))
                       <|> (string "newline" >> (return . Character $ '\n')))
   where charPrefix = char '#' >> char '\\'

-- SKIPPING for now. see
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
-- parseFloat :: Parser LispVal
-- parseFloat =

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
   head <- endBy parseExpr spaces
   tail <- char '.' >> spaces >> parseExpr
   return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= \x -> return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseString
   {-<|> parseFloat-}
   <|> parseQuoted
   <|> parseNumber
   <|> parseChar
   <|> parseAtom
   <|> do char '('
          x <- try parseList <|> parseDottedList
          char ')'
          return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
   Left err -> String $ "No match: " ++ show err
   Right val -> val
