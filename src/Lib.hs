module Lib
    ( readExpr,
      eval,
      extractValue,
      trapError,
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import Numeric
import Data.Typeable

{- No problem parsing strings, they just have to be entered right
 - sush as :  '(+ 1 \"foo\")' will work in powershell -}

------------------------------------------------------------------------
-- Error stuff
------------------------------------------------------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- AFAIK to convert this just change import to Control.Monad.Except and
-- delete the following three lines

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

------------------------------------------------------------------------
-- AST
------------------------------------------------------------------------

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
showVal (Character contents) = "#" ++ [contents]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . "
                          ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

------------------------------------------------------------------------
-- Evaluating stuff
------------------------------------------------------------------------

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- decided to always just and the operation if done on a list because
-- not sure if this behavior is even expected and this seemed like the
-- safest option
boolOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
boolOp op [] = throwError $ NumArgs 1 []
boolOp op [param] = return . Bool . op $ param
boolOp op multiArgs = throwError $ NumArgs 1 multiArgs

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

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction
                         "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args)) =  mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
