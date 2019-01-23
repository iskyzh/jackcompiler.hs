module ParserExpressions where

import Tokenizer
import TokenParser
import ParserUtil

pExpressionList :: Parser ParseResult
pExpressionList = do
    expressionList <- pExpression `sepby0` (ParseNode <$> sat (== Symbol ','))
    return ParseTree {
        what = "expressionList",
        children = expressionList
    }

pExpression :: Parser ParseResult
pExpression = do
    terms <- pTerm `sepby1` pOp
    return ParseTree {
        what = "expression",
        children = terms
    }

pTerm :: Parser ParseResult
pTerm = ((wrap . ParseNode) <$> ((sat isIntegerConstant) +++ (sat isStringConstant)))
        +++ (wrap <$> pKeywordConstant)
        +++ (wrapChildren <$> pSubroutineCall)
        +++ do 
            varName <- pVarName
            expression <- pBracket '[' ']' pExpression
            return $ ParseTree {
                what = "term",
                children = varName : expression
            }
        +++ (wrapChildren <$> pBracket '(' ')' pExpression)
        +++ (wrap <$> pVarName)
        +++ do
            unaryOp <- pUnaryOp
            term <- pTerm
            return ParseTree {
                what = "term",
                children = [unaryOp, term]
            }
    where 
        wrap child = ParseTree {
            what = "term",
            children = [child]
        }
        wrapChildren children = ParseTree {
            what = "term",
            children = children
        }

pKeywordConstant :: Parser ParseResult
pKeywordConstant = ParseNode <$> sat (\x -> x == Keyword "true" || x == Keyword "false" || x == Keyword "null" || x == Keyword "this")

pOp :: Parser ParseResult
pOp = ParseNode <$> sat pOp' where
    pOp' (Symbol sym) = sym `elem` "+-*/&|<>="
    pOp' _ = False

pUnaryOp :: Parser ParseResult
pUnaryOp = ParseNode <$> sat pUnaryOp' where
    pUnaryOp' (Symbol sym) = sym `elem` "-~"
    pUnaryOp' _ = False

pSubroutineCall :: Parser [ParseResult]
pSubroutineCall = do
        subroutineName <- pSubroutineName
        expressionList <- pBracket '(' ')' pExpressionList
        return $ subroutineName : expressionList
    +++ do
        classOrVarName <- pClassName +++ pVarName
        sym <- ParseNode <$> sat (== Symbol '.')
        subroutineName <- pSubroutineName
        expressionList <- pBracket '(' ')' pExpressionList
        return $ classOrVarName : sym : subroutineName : expressionList
