module Lib
    ( readExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

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
showVal (Character contents) = "#" ++ show contents
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . "
                          ++ showVal tail ++ ")"

instance Show LispVal where show = showVal


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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
   <|> (char '#' >> (parseDec <|> parseOct <|> parseHex))

-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber = do
--    num <- many1 digit
--    return (Number . read $ num)

parseChar :: Parser LispVal
parseChar = charPrefix >> ((anyChar >>= (return . Character))
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
   <|> parseNumber
   {-<|> parseFloat-}
   <|> parseChar
   <|> parseQuoted
   <|> parseAtom
   <|> do char '('
          x <- try parseList <|> parseDottedList
          char ')'
          return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
   Left err -> "No match: " ++ show err
   Right val -> "Found " ++ show val
