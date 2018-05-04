module Main where
import System.Environment

import Lib

main :: IO ()
main = getArgs >>= print . eval . readExpr . head


