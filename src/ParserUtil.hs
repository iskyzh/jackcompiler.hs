module ParserUtil where

import           Tokenizer
import           TokenParser

pBracket :: Char -> Char -> Parser ParseResult -> Parser [ParseResult]
pBracket l r parser = do
        symL <- ParseNode <$> sat (== Symbol l)
        p    <- parser
        symR <- ParseNode <$> sat (== Symbol r)
        return [symL, p, symR]

pBrackets :: Char -> Char -> Parser [ParseResult] -> Parser [ParseResult]
pBrackets l r parser = do
        symL <- ParseNode <$> sat (== Symbol l)
        p    <- parser
        symR <- ParseNode <$> sat (== Symbol r)
        return $ [symL] ++ p ++ [symR]

pEnd :: Parser ParseResult
pEnd = ParseNode <$> sat (== Symbol ';')

pClassName :: Parser ParseResult
pClassName = ParseNode <$> sat isIdentifier

pSubroutineName :: Parser ParseResult
pSubroutineName = ParseNode <$> sat isIdentifier

pVarName :: Parser ParseResult
pVarName = ParseNode <$> sat isIdentifier
