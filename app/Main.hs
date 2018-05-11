module Main where
import System.Environment
import Control.Monad (liftM)
import System.IO

import Eval
import Parse
import Error

flushStr  :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return . extractValue
                  $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
   result <- prompt
   if predicate result
      then return ()
      else action result >> until_ predicate prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Schemer>>> ") evalAndPrint

main :: IO ()
main =  do args <- getArgs
           case length args of
                0 -> runRepl
                1 -> evalAndPrint $ args !! 0
                otherwise -> putStrLn "Program takes only 0 or 1 argument"
