module Common
  ( (?)
  , (&&&), (|||), nott
  )
where

----------------------------------------------------------------------
{- useful combinators -}

infix 1 ?
(?) :: Bool -> p -> p -> p
c ? t = \f -> if c then t else f

infixr 3 &&&
(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(&&&) = s' (&&)

infixr 2 |||
(|||) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(|||) = s' (||)

nott = (not .)
nott :: (a -> Bool) -> a -> Bool

s' :: (t1 -> t2 -> t3) -> (t4 -> t1) -> (t4 -> t2) -> t4 -> t3
s' f g h x = f (g x) (h x)
