module ParserStatements where

import           Tokenizer
import           TokenParser
import           ParserExpressions
import           ParserUtil

pStatements :: Parser ParseResult
pStatements = do
        statements <- many0 pStatement
        return ParseTree { what = "statements", children = statements }

pStatement :: Parser ParseResult
pStatement =
        pLetStatement
                +++ pIfStatement
                +++ pWhileStatement
                +++ pDoStatement
                +++ pReturnStatement

pLetStatement :: Parser ParseResult
pLetStatement = do
        kwd          <- ParseNode <$> sat (== Keyword "let")
        varName      <- pVarName
        expressionIn <- optional $ pBracket '[' ']' pExpression
        sym1         <- ParseNode <$> sat (== Symbol '=')
        expression   <- pExpression
        sym2         <- ParseNode <$> sat (== Symbol ';')
        return ParseTree
                { what     = "letStatement"
                , children = [kwd, varName]
                             ++ expressionIn
                             ++ [sym1, expression, sym2]
                }

pIfStatement :: Parser ParseResult
pIfStatement = do
        kwd        <- ParseNode <$> sat (== Keyword "if")
        expression <- pBracket '(' ')' pExpression
        statements <- pBracket '{' '}' pStatements
        whatelse   <- optional $ do
                kwd        <- ParseNode <$> sat (== Keyword "else")
                statements <- pBracket '{' '}' pStatements
                return $ kwd : statements
        return ParseTree
                { what     = "ifStatement"
                , children = [kwd] ++ expression ++ statements ++ whatelse
                }

pWhileStatement :: Parser ParseResult
pWhileStatement = do
        kwd        <- ParseNode <$> sat (== Keyword "while")
        expression <- pBracket '(' ')' pExpression
        statements <- pBracket '{' '}' pStatements
        return ParseTree { what     = "whileStatement"
                         , children = kwd : expression ++ statements
                         }

pDoStatement :: Parser ParseResult
pDoStatement = do
        kwd            <- ParseNode <$> sat (== Keyword "do")
        subroutineCall <- pSubroutineCall
        end            <- pEnd
        return ParseTree { what     = "doStatement"
                         , children = kwd : subroutineCall ++ [end]
                         }
pReturnStatement :: Parser ParseResult
pReturnStatement = do
        kwd        <- ParseNode <$> sat (== Keyword "return")
        expression <- optional1 pExpression
        end        <- pEnd
        return ParseTree { what     = "returnStatement"
                         , children = kwd : expression ++ [end]
                         }
