module CodeWriter (
    writeCode,
    SymbolTable(..)
) where

import TokenParser
import Tokenizer
import Data.Map.Strict

data SymbolTable = ST { localSymbols :: Map String Int
                        }

findSymbol :: SymbolTable -> String -> String
findSymbol (ST localSymbols) symbol
    | member symbol localSymbols = "local " ++ show (localSymbols ! symbol)
    | otherwise = error "symbol not found"

writeCode :: SymbolTable -> ParseResult -> [String]

writeCode symbolTable (ParseTree "expression" children) = [""]
writeCode symbolTable (ParseTree "term" ((ParseNode (Identifier symbol)):_)) = ["push " ++ (findSymbol symbolTable symbol)]
