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
             | Float Float

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
parseNumPrefix :: String -> (String -> [(Integer, String)]) -> Parser LispVal
parseNumPrefix (hash:letter:_) reader = do
      char hash
      char letter
      num <- many1 digit
      let [(val, _)] = reader num
      return . Number $ val

parseDec :: Parser LispVal
parseDec = ((many1 digit) >>= (return . Number . read))
   <|> parseNumPrefix "#b" readDec

-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber = do
--    num <- many1 digit
--    return (Number . read $ num)

parseOct :: Parser LispVal
parseOct = parseNumPrefix "#o" readOct

parseHex :: Parser LispVal
parseHex = parseNumPrefix "#x" readHex

parseNumber :: Parser LispVal
parseNumber = parseDec
   <|> parseOct
   <|> parseHex

parseChar :: Parser LispVal
parseChar = charPrefix >> ((anyChar >>= (return . Character))
                       <|> (string "space"   >> (return . Character $ ' '))
                       <|> (string "newline" >> (return . Character $ '\n')))
   where charPrefix = char '#' >> char '\\'

-- SKIPPING for now. see
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
-- parseFloat :: Parser LispVal
-- parseFloat =

parseExpr :: Parser LispVal
parseExpr = parseAtom
   <|> parseString
   <|> parseNumber
   {-<|> parseFloat-}
   <|> parseChar


spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
   Left err -> "No match: " ++ show err
   Right val -> "Found value"
