module Parse
    ( readExpr
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

import Numeric

import AST
import Error

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
   Left err -> throwError $ Parser err
   Right val -> return val
