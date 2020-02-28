module Main where

import           KParser
import           Parser
import           AST
import           System.Environment
import           System.IO
import           LLVMGen
import Debug.Trace (trace)

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse (x:xs) = handleFile x
parse [] = error "Invalid arguments"

handleFile :: String -> IO ()
handleFile filename = do
  file <- openFile filename ReadMode
  content <- hGetContents file
  startKoak content $ filename ++ ".ir"

