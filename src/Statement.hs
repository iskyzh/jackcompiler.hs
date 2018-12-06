module Statement
    ( Statement(..)
    )
where

import           Expression

data Statement = Statements [Statement]
    | LetStatement String Expression Expression