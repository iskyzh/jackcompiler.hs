{-# LANGUAGE TemplateHaskell #-}
module TokenParser where

import Control.Applicative
import Control.Monad
import Data.DeriveTH
import Tokenizer

derive makeIs ''JackToken

newtype Parser a = Parser { parse :: ([JackToken] -> [(a, [JackToken])]) }

item :: Parser JackToken
item = Parser (\cs -> case cs of
                      [] -> []
                      (c:cs) -> [(c, cs)])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus
    
instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f = Parser (\cs -> concat 
                    [parse (f a) cs' | 
                        (a, cs') <- parse p cs])

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                              []     -> []
                              (x:xs) -> [x])

sat :: (JackToken -> Bool) -> Parser JackToken
sat p = do { c <- item; if p c then return c else mzero }

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                    rest a = (do f <- op
                                 b <- p
                                 rest (f a b))
                             +++ return a

data ParseResult = ParseResult {  what :: String,
                                  children :: [ParseResult] } 
                 | ParseNode {  what :: String,
                                content :: JackToken }
    deriving(Show, Eq)

expression :: Parser ParseResult
expression = term `chainl1` op

term :: Parser ParseResult
term = checkIf isIntegerConstant "integerConstant"
        +++ checkIf isStringConstant "stringConstant"
        +++ do { k <- keywordConstant ; return $ ParseNode "keywordConstant" k }
        +++ do { id <- sat isIdentifier ; return $ ParseNode "varName" id }

checkIf :: (JackToken -> Bool) -> String -> Parser ParseResult
checkIf cond what = do { p <- sat cond ; return $ ParseNode what p }

keywordConstant :: Parser JackToken
keywordConstant = sat (== Keyword "true") 
                    +++ sat (== Keyword "false")
                    +++ sat (== Keyword "null")
                    +++ sat (== Keyword "this")

subroutineCall :: Parser ParseResult
subroutineCall = undefined

op :: Parser (ParseResult -> ParseResult -> ParseResult)
op = do { o <- sat isOp ; return $ mergeResult o } where
    isOp :: JackToken -> Bool
    isOp (Symbol x) = x `elem` symbols where symbols = "+-*/&|<>="
    isOp _ = False

mergeResult :: JackToken -> ParseResult -> ParseResult -> ParseResult
mergeResult symb lnode@(ParseNode _ _) rnode@(ParseNode _ _) = ParseResult "term" [lnode, ParseNode "op" symb, rnode]
mergeResult symb (ParseResult what children) rnode@(ParseNode _ _) = ParseResult what (children ++ [ParseNode "op" symb] ++ [rnode])
mergeResult symb lnode@(ParseNode _ _) (ParseResult what children) = ParseResult what (lnode : [ParseNode "op" symb] ++ children)
mergeResult symb (ParseResult whatL childrenL) (ParseResult whatR childrenR) = ParseResult whatL (childrenL ++ [ParseNode "op" symb] ++ childrenR)
