{-# LANGUAGE ExistentialQuantification #-}

module Eval
    ( eval,
      Env,
      nullEnv
    ) where

import Control.Monad
import Control.Monad.Except
import Data.Char (toLower)
import Data.List (genericReplicate)
import Data.IORef

import AST
import Error

{- No problem parsing strings, they just have to be entered right
 - sush as :  '(+ 1 \"foo\")' will work in powershell -}

------------------------------------------------------------------------
-- IORef
------------------------------------------------------------------------

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True)
                                                 . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
   env <- liftIO $ readIORef envRef
   maybe (throwError $ UnboundVar "Getting an unbound variable" var)
         (liftIO . readIORef)
         (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
   env <- liftIO $ readIORef envRef
   maybe (throwError $ UnboundVar "Setting an unbound variable" var)
         (liftIO . (flip writeIORef value))
         (lookup var env)
   return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
   alreadyDefined <- liftIO $ isBound envRef var
   if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
         valueRef <- newIORef value
         env <- readIORef envRef
         writeIORef envRef ((var, valueRef) : env)
         return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
   where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
         addBinding (var, value) = do ref <- newIORef value
                                      return (var, ref)
------------------------------------------------------------------------
-- Evaluating stuff
------------------------------------------------------------------------

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackAtom :: LispVal -> ThrowsError String
unpackAtom (Atom a) = return a
unpackAtom notAtom = throwError $ TypeMismatch "atom" notAtom

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character c) = return c
unpackChar (String [c]) = return c
unpackChar (Number n) = if length numStr == 1
                           then return $ head numStr
                           else throwError $ TypeMismatch "char" $ Number n
                        where numStr = show n
unpackChar notChar = throwError $ TypeMismatch "char" notChar


numericBinop :: (Integer -> Integer -> Integer) -> SchemeFunc
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal]
             -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                                 first  <- unpacker $ args !! 0
                                 second <- unpacker $ args !! 1
                                 return . Bool $ op first second

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolOp :: (LispVal -> Bool) -> SchemeFunc
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


type SchemeFunc = [LispVal] -> ThrowsError LispVal

car :: SchemeFunc
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: SchemeFunc
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: SchemeFunc
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return . List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: SchemeFunc
eqv [(Bool arg1), (Bool arg2)]             = return . Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return . Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return . Bool $ arg1 == arg2
eqv [(Character arg1), (Character arg2)]   = return . Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return . Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return . Bool $ (length arg1 == length arg2)
                                 && (all eqvPair $ zip arg1 arg2)
   where eqvPair (x1, x2) = case eqv [x1, x2] of
                              Left err -> False
                              Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
   do unpacked1 <- unpacker arg1
      unpacked2 <- unpacker arg2
      return $ unpacked1 == unpacked2
   `catchError` (const $ return False)

equal :: SchemeFunc
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x],
                                                      List $ ys ++ [y]]
equal [(List arg1), (List arg2)] = return . Bool $ (length arg1 == length arg2)
                                  && (all equalPair $ zip arg1 arg2)
   where equalPair (x1, x2) = case equal [x1, x2] of
                                   Left err -> False
                                   Right (Bool val) -> val
equal [arg1, arg2] = do
   primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr,
                       AnyUnpacker unpackBool]
   eqvEquals <- eqv [arg1, arg2]
   return . Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

cmpNoCase :: (String -> String -> Bool) -> (String -> String -> Bool)
cmpNoCase op = (\s1 s2 -> op (map toLower s1) (map toLower s2))

strlen :: SchemeFunc
strlen [s] = unpackStr s >>= return . Number . toInteger . length
strlen badArgs = throwError $ NumArgs 1 badArgs

strSym :: SchemeFunc
strSym [s] = unpackStr s >>= return . Atom
strSym badArgs = throwError $ NumArgs 1 badArgs

symStr :: SchemeFunc
symStr [a] = unpackAtom a >>= return . String

makeStr :: SchemeFunc
makeStr [len] = makeStr [len, Character '\0']
makeStr [len, c] = do
   n <- unpackNum len
   c <- unpackChar c
   return . String $ genericReplicate n c
makeStr badArgs = throwError $ NumArgs 1 badArgs

strRef :: SchemeFunc
strRef [str, k] = do
   s <- unpackStr str
   i <- unpackNum k
   if length s < fromIntegral i
      then throwError $ BadIndex str k
      else return . Character $ s !! fromIntegral i

buildStr :: SchemeFunc
buildStr chars = mapM unpackChar chars >>= return . String


primitives :: [(String, SchemeFunc)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci=?", strBoolBinop $ cmpNoCase (==)),
              ("string-ci<?", strBoolBinop $ cmpNoCase (<)),
              ("string-ci>?", strBoolBinop $ cmpNoCase (>)),
              ("string-ci<=?", strBoolBinop $ cmpNoCase (<=)),
              ("string-ci>=?", strBoolBinop $ cmpNoCase (>=)),
              ("string->symbol", strSym),
              ("symbol->string", symStr),
              ("string-length", strlen),
              ("make-string", makeStr),
              ("string-ref", strRef),
              ("string", buildStr),
              ("symbol?", boolOp isSym),
              ("string?", boolOp isStr),
              ("number?", boolOp isNum),
              ("bool?", boolOp isBool),
              ("char?", boolOp isChar),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)
             ]

apply :: String -> SchemeFunc
apply func args = maybe (throwError $ NotFunction
                         "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


evalTest :: Env -> LispVal -> IOThrowsError Bool
evalTest _ (Atom "else") = return True
evalTest env test = liftThrows . unpackBool =<< eval env test

evalCondResult :: Env -> Bool -> [LispVal] -> IOThrowsError LispVal
evalCondResult _ b [] = return $ Bool b
evalCondResult env b xs = return . last =<< mapM (eval env) xs

cond :: Env -> [LispVal] -> IOThrowsError LispVal
cond _ [] = throwError $ UnendedExpr "cond terminated without result"
cond env (List (test:xs) : rest) = do
   result <- evalTest env test
   if result then evalCondResult env result xs
             else cond env rest
cond _ (badArg : rest) = throwError $ TypeMismatch "pair" badArg

checkClause :: LispVal -> [LispVal] -> ThrowsError Bool
checkClause key [] = return False
checkClause key (x:xs) = do
   (Bool result) <- eqv [key, x]
   if result then return True
             else checkClause key xs

caseEval :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
caseEval _ key [] = throwError $ UnendedExpr "case terminated without result"
caseEval env key (List ((List datums):exprs):rest) = do
   foundMatch <- liftThrows $ checkClause key datums
   if foundMatch then return . last =<< mapM (eval env) exprs
                 else caseEval env key rest
caseEval _ key (List (badCheck:exprs):rest) = throwError $ TypeMismatch "case clause"
                                                                         badCheck
caseEval _ key badArgs = throwError $ TypeMismatch "list" $ List badArgs


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
-- variables
eval env (Atom id) = getVar env id
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
 -- conditionals
eval env (List [Atom "if", pred, conseq, alt]) =
   do result <- eval env pred
      case result of
         Bool False -> eval env alt
         Bool True  -> eval env conseq
         otherwise  -> throwError $ TypeMismatch "boolean" result
-- cond
eval env (List ((Atom "cond") : clauses)) = cond env clauses
-- case
eval env (List ((Atom "case") : key : rest)) = do k <- eval env key
                                                  caseEval env k rest
-- list
eval env (List [Atom "quote", val]) = return val
-- dotted list
eval env (List (Atom func:args)) =  mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

