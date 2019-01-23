module Main where

import Lib
import Tokenizer
import TokenParser
import ParserProgramStructure

main :: IO ()
main = interact func where
    func x = parseXML (parse pClass $ tokenize x)
