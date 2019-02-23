module CodeWriter (
    writeCode,
    SymbolTable(..)
) where

import TokenParser
import Tokenizer
import Data.Map.Strict

data SymbolTable = ST { local :: Map String Int
                        }

findSymbol :: SymbolTable -> String -> String
findSymbol (ST local) symbol
    | member symbol local = "local " ++ show (local ! symbol)
    | otherwise = error "symbol not found"

writeCode :: SymbolTable -> ParseResult -> [String]

writeCode symbolTable (ParseTree "expression" children) = [""]
writeCode symbolTable (ParseTree "term" ((ParseNode (Identifier symbol)):_)) = ["push " ++ (findSymbol symbolTable symbol)]
