module Error
   ( LispError(..),
     ThrowsError,
     trapError,
     extractValue,
     throwError,
     catchError
   ) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)

import AST

------------------------------------------------------------------------
-- Error stuff
------------------------------------------------------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | UnendedExpr String
               | BadIndex LispVal LispVal
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
showError (UnendedExpr message)         = "No else found; " ++ message
showError (BadIndex string index)       = "Bad index " ++ show index ++ " for "
                                       ++ show string

instance Show LispError where show = showError

-- AFAIK to convert this just change import to Control.Monad.Except and
-- delete the following three lines

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

