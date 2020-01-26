module Main where

import           KParser
import           Parser
import           AST

main :: IO ()
main = interact inputParsingExpression

inputParsingExpression :: String -> String
inputParsingExpression input = show $ runParser koak input
