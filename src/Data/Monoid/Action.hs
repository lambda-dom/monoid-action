{- |
Module: Data.Monoid.Action

The monoid @Action@ typeclass.
-}

module Data.Monoid.Action (
    -- * Typeclasses.
    Action (..),

    -- * The Free @m@-action left adjoint.
    Free,

    -- ** Adjunction maps.
    free,
    coextract,

    -- * The Cofree @m@-action right adjoint.
    Cofree,

    -- ** Adjunction maps.
    copure,
    cofree,
) where

-- Imports.
-- Base.
import Data.Monoid (Endo (..))
import Data.Void (Void)

-- Libraries.
import Control.Comonad (Comonad (..))


{- |
The type class for a (left) 'Monoid' action. It must satisfy the following two laws:

__Identity__:

prop> mempty |*> x = x

__Associativity__:

prop> m |*> n |*> x = m <> n |*> x
-}
class Monoid m => Action m a where
    (|*>) :: m -> a -> a
    infixr 5 |*>


{- | Endomorphisms action. -}
instance Action (Endo a) a where
    (|*>) (Endo f) = f

{- | Action of a monoid @m@ on itself. -}
instance Monoid m => Action m m where
    (|*>) = (<>)

{- | The (unique) action on the terminal. -}
instance Monoid m => Action m () where
    (|*>) _ = id

{- | The (unique) action on the initial 'Void'. -}
instance Monoid m => Action m Void where
    (|*>) _ = id

{- | Product of actions. -}
instance (Action m a , Action m b) => Action m (a, b) where
    (|*>) m (x, y) = (m |*> x, m |*> y)

{- | Coproduct of actions. -}
instance (Action m a , Action m b) => Action m (Either a b) where
    (|*>) m (Left x) = Left (m |*> x)
    (|*>) m (Right y) = Right (m |*> y)

{- | Pointwise action on action-valued functions. -}
instance Action m b => Action m (a -> b) where
    (|*>) m f x = m |*> f x

{- | Lift an action to 'Maybe'. -}
instance Action m a => Action m (Maybe a) where
    (|*>) m = fmap (m |*>)


{- | The free @m@-action on @a@; isomorphic to @Writer m a@. -}
data Free m a = Free m a
    deriving stock Functor


instance Monoid m => Action m (Free m a) where
    (|*>) m (Free n x) = Free (m <> n) x

instance Monoid m => Applicative (Free m) where
    {- | The unit of the 'Free' adjunction. -}
    pure :: a -> Free m a
    pure = Free mempty

    (<*>) :: Free m (a -> b) -> Free m a -> Free m b
    (<*>) (Free m f) (Free n x) = Free (m <> n) (f x)

{- | The monad induced by the 'Free' adjunction. -}
instance Monoid m => Monad (Free m) where
    (>>=) :: Free m a -> (a -> Free m b) -> Free m b
    (>>=) (Free m x) h = m |*> h x


{- | Universal property of the unit of the 'Free' adjunction. -}
free :: (Action m b) => (a -> b) -> Free m a -> b
free f (Free m x) = m |*> f x

{- | The counit of the 'Free' adjunction. -}
coextract :: Action m b => Free m b -> b
coextract (Free m x) = m |*> x


{- |
The cofree @m@-action on @a@, isomorphic to @Reader m a@.

The underlying type is @m -> a@ with action:

@
    m |*> f = f . ( <> m)
@
-}
newtype Cofree m a = Cofree (m -> a)
    deriving stock Functor

instance Monoid m => Action m (Cofree m a) where
    (|*>) m (Cofree f) = Cofree (f . (<> m))

{- |
The 'Comonad' instance.

The 'extract' map has formal properties similar to those of the integral map \(f\mapsto \int f\).
If such a map is to be /equivariant/ when @a@ is an action, and denoting the actions by \(\cdot\),
then,

\[
 \int_{M} f(m) = \int_{M} f(m 1) = \int_{M} (m\cdot f)1 = (\int_{M} m)\cdot f(1)
\]

so that \(\int f\) is, up to a multiple, just evaluation on the monoid identity. If the integral
map itself is to be equivariant then that leaves \(\int_{M} f(m) = 1\) as the only option, and this
gives the definition of 'extract'.
-}
instance Monoid m => Comonad (Cofree m) where
    {- | The counit of the 'Cofree' adjunction.

    -}
    extract :: Cofree m a -> a
    extract (Cofree f) = f mempty

    duplicate :: Cofree m a -> Cofree m (Cofree m a)
    duplicate (Cofree f) = Cofree $ \ m -> Cofree $ \ n -> f (m <> n)


{- | The unit of the 'Cofree' adjunction. -}
copure :: Action m b => b -> Cofree m b
copure x = Cofree (|*> x)

{- | Universal property of the counit of the 'Cofree' adjunction. -}
cofree :: Action m b => (b -> a) -> b -> Cofree m a
cofree f x = Cofree $ f . (|*> x)
