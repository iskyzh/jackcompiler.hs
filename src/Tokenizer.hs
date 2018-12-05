module Tokenizer
    ( JackToken(..)
    , tokenize
    )
where

import           Data.Char                      ( isDigit
                                                , isLetter
                                                , isSpace
                                                )

data JackToken = Keyword String
               | Symbol Char
               | IntegerConstant Int
               | StringConstant String
               | Identifier String
               deriving (Show, Eq)

tokenize :: String -> [JackToken]

tokenize (x : xs) | x `elem` symbols    = Symbol x : tokenize xs
                  | x == '"'            = tokenizeString xs
                  | isDigit x           = tokenizeInteger allTokens
                  | isIdentifierBegin x = tokenizeIdentifier allTokens
                  | isSpace x           = tokenize xs
                  | otherwise           = undefined
  where
    symbols   = "{}()[].,;+-*/&|<>=-"
    allTokens = (x : xs)

tokenize _ = []

tokenizeString :: String -> [JackToken]
tokenizeString str = tokenizeString' str ""  where
    tokenizeString' (x : xs) str | x == '\"' = StringConstant str : tokenize xs
                                 | otherwise = tokenizeString' xs (str ++ [x])
    tokenizeString' _ str = [StringConstant str]

tokenizeInteger :: String -> [JackToken]
tokenizeInteger str = tokenizeInteger' str 0  where
    tokenizeInteger' (x : xs) num
        | isDigit x = tokenizeInteger' xs (num * 10 + (read [x]) :: Int)
        | otherwise = IntegerConstant num : tokenize (x : xs)
    tokenizeInteger' _ num = [IntegerConstant num]

tokenizeIdentifier :: String -> [JackToken]
tokenizeIdentifier str = tokenizeIdentifier' str ""  where
    tokenizeIdentifier' (x : xs) id
        | isIdentifier x = tokenizeIdentifier' xs (id ++ [x])
        | otherwise      = recognize id : tokenize (x : xs)
    tokenizeIdentifier' _ id = [recognize id]

    recognize id = case isKeyword id of
        True  -> Keyword id
        False -> Identifier id
    isKeyword id = id `elem` keywords

    keywords =
        [ "class"
        , "constructor"
        , "function"
        , "method"
        , "field"
        , "static"
        , "var"
        , "int"
        , "char"
        , "boolean"
        , "void"
        , "true"
        , "false"
        , "null"
        , "this"
        , "let"
        , "do"
        , "if"
        , "else"
        , "while"
        , "return"
        ]

isIdentifier :: Char -> Bool
isIdentifier x = isLetter x || isDigit x || x == '_'

isIdentifierBegin :: Char -> Bool
isIdentifierBegin x = isLetter x || x == '_'
