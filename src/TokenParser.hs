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
                                content :: String }

{-
expression :: Parser ParseResult
expression = term `chainl1` term

term :: Parser ParseResult
term = do {}
-}

keywordConstant :: Parser JackToken
keywordConstant = sat (== Keyword "true") 
                    +++ sat (== Keyword "false")
                    +++ sat (== Keyword "null")
                    +++ sat (== Keyword "this")
