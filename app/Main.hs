module Main where
import System.Environment
import Control.Monad

import Lib

main :: IO ()
main =  do
   args <- getArgs
   evaled <- return $ liftM show $ readExpr (head args) >>= eval
   putStrLn $ extractValue $ trapError evaled
