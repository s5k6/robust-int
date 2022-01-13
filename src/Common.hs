module Common
  ( (?)
  , (&&&), (|||), nott
  )
where

----------------------------------------------------------------------
-- useful combinators

infixr 3 &&&
(&&&) :: Applicative f => f Bool -> f Bool -> f Bool
f &&& g = (&&) <$> f <*> g

infixr 2 |||
(|||) :: Applicative f => f Bool -> f Bool -> f Bool
f ||| g = (||) <$> f <*> g

nott :: Applicative f => f Bool -> f Bool
nott = fmap not

infix 1 ?
(?) :: Bool -> p -> p -> p
c ? t = \f -> if c then t else f
