{- |
Module: Data.Monoid.Action

The monoid @Action@ typeclass.
-}

module Data.Monoid.Action (
    -- * Typeclasses.
    Action (..),
) where


{- |
The type class for a left 'Monoid' action. It must satisfy the following two laws:

__Identity__:

prop> mempty |*> x = x

__Associativity__:

prop> m |*> n |*> x = m <> n |*> x
-}
class Monoid m => Action m a where
    (|*>) :: m -> a -> a
    infixr 5 |*>
