module Proposition(Proposition(..))
where

data Proposition = 
                Constant Bool|
                Variable String|
                Negation Proposition|
                Conjuction Proposition Proposition|
                Disjunction Proposition Proposition|
                Conditional Proposition Proposition|
                Biconditional Proposition Proposition
                deriving (Show)