module Lab08 where

import Lab08Def

-- Implement the following methods for Forky. See the PDF for what to aim for.

instance Functor Forky where
    -- fmap :: (a -> b) -> Forky a -> Forky b
    fmap f (Tip x) = Tip (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)
instance Applicative Forky where
    -- pure :: a -> Forky a
    pure a = Tip a
    -- (<*>) :: Forky (a -> b) -> Forky a -> Forky b
    (Tip tf) <*> t = fmap tf t
    (Branch tf1 tf2) <*> t = Branch (tf1 <*> t) (tf2 <*> t)
    
