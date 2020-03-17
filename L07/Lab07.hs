module Lab07 where

import Lab07Def

{- [6 marks]

   Help me make Weight an instance of Num so I can do basic arithmetic.
   No need to do (-), default implementation uses (+) and negate.
-}
instance Num Weight where
    -- (+) :: Weight -> Weight -> Weight
    (Fin a) + (Fin b) = Fin (a + b)
    (Fin a) + Inf = Inf
    Inf + (Fin a) = Inf
    Inf + Inf = Inf
    -- Corner case: anything + Inf = Inf.  The other order too.

    -- negate :: Weight -> Weight
    -- Corner case done because unsupported:
    negate Inf = error "negative infinity not supported"
    negate (Fin a) = Fin (0 - a)
    -- (*) :: Weight -> Weight -> Weight
    -- Corner cases:
    -- (zero or negative) * Inf is unsupported
    (Fin a) * (Fin b) = Fin (a * b)
    positive * Inf 
        | positive > 0 = Inf
        | positive == 0 = error "not supported"
        | positive < 0 = error "not supported"
    -- Don't forget the other order.
    Inf * positive
        | positive > 0 = Inf
        | positive == 0 = error "not supported"
        | positive < 0 = error "not supported"
    -- abs :: Weight -> Weight
    abs (Fin a)
        | a >= 0 = Fin a
        | otherwise = negate (Fin a)
    abs Inf = Inf
    -- signum :: Weight -> Weight
    -- Corner case: signum Inf is positive one.
    signum Inf = 1
    signum (Fin a)
        | a > 0 = Fin 1
        | a == 0 = Fin 0
        | a < 0 = Fin (-1)

    -- fromInteger :: Integer -> Weight
    fromInteger a = Fin a
