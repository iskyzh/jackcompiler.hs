module CodeWriter (
    writeCode,
    buildSymbolTable,
    SymbolTable(..),
    TypeTable(..)
) where

import TokenParser
import Tokenizer
import Data.Map.Strict

data TypeTable = Primitive String | UserClass String

type SymbolSingleTable = Map String (Int, TypeTable)

data SymbolTable = ST { static :: SymbolSingleTable,
                        local :: SymbolSingleTable,
                        argument :: SymbolSingleTable,
                        field :: SymbolSingleTable
                        }

findSymbol :: SymbolTable -> String -> String
findSymbol (ST static local argument field) symbol
    | member symbol local = "local " ++ show (number (local ! symbol))
    | otherwise = error "symbol not found" where
        number = fst

writeCode :: SymbolTable -> ParseResult -> [String]

writeCode symbolTable (ParseTree "expression" children) = takeTerm children where
    writeTerm = writeCode symbolTable
    writeOp (ParseNode (Symbol symb)) = case symb of
        '+' -> "add"
        '-' -> "sub"
        '*' -> "mul"
        '/' -> "div"
        '&' -> "and"
        '|' -> "or"
        '<' -> "lt"
        '>' -> "gt"
        '=' -> "eq"
        _ -> error "unsupported operator"
    takeTerm (term:xs) = writeTerm term ++ takeOpTerm xs
    takeTerm [] = []
    takeOpTerm (op:term:xs) = writeTerm term ++ [writeOp op] ++ takeOpTerm xs
    takeOpTerm [] = []
    takeOpTerm _ = error "unexpected end when reading next operator"

writeCode symbolTable (ParseTree "term" ((ParseNode token):_)) = ["push " ++ queryToken token] where
    queryToken (Identifier symbol)= findSymbol symbolTable symbol
    queryToken (IntegerConstant integer)= "constant " ++ show integer
    queryToken _ = error "unsupported term token"

writeCode symbolTable (ParseTree "expressionList" expressions) = writeExpressions expressions where
    writeExpressions (expression@(ParseTree "expression" _):xs) = writeCode symbolTable expression ++ writeExpressions xs
    writeExpressions (_:xs) = writeExpressions xs
    writeExpressions _ = []

writeCode _ a = error $ "unsupported code" ++ show a

buildSymbolTable = undefined