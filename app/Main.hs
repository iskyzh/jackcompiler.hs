module Main where

import Lib
import Tokenizer

main :: IO ()
main = interact $ parseTokenXML . tokenize
