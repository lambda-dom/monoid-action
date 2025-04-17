{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Data.Monoid.Action

The monoid @Action@ typeclass.
-}

module Data.Monoid.Action (
    -- * Typeclasses.
    Action (..),

    -- ** Functions.
    transfer,

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

    -- * Destructuring @('Integral' w, 'FiniteBits' w)@.
    BitArray (..),
    ByteArray (..),
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits, Bits)
import Data.Ix (Ix)
import Data.Monoid (Endo (..))
import Data.Void (Void)
import Data.Word (Word8)

-- Libraries.
import Control.Comonad (Comonad (..))
import Data.Sequence (Seq)
import qualified Data.Vector as Vect (Vector)
import qualified Data.Vector.Strict as SVect (Vector)
import qualified Data.Vector.Unboxed as UVect (Vector, Unbox, map)
import qualified Data.Vector.Storable as StVect (Vector, Storable, map)
import qualified Data.Array.IArray as Array (Array)
import qualified Data.ByteString as Bytes (ByteString, map)
import qualified Data.ByteString.Lazy as LBytes (ByteString, map)
import qualified Data.ByteString.Short as SBytes (ShortByteString, map)
import qualified Data.Text as Text (Text, map)
import qualified Data.Text.Lazy as LText (Text, map)

-- Package.
import qualified Data.Monoid.Action.Bits as Bits (pack, bits)
import qualified Data.Monoid.Action.Bytes as Bytes (pack, bytes)


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
    (|*>) :: Endo a -> a -> a
    (|*>) (Endo f) = f

{- | Action of a monoid @m@ on itself. -}
instance Monoid m => Action m m where
    (|*>) :: m -> m -> m
    (|*>) = (<>)

{- | The (unique) action on the terminal. -}
instance Monoid m => Action m () where
    (|*>) :: m -> () -> ()
    (|*>) _ = id

{- | The (unique) action on the initial 'Void'. -}
instance Monoid m => Action m Void where
    (|*>) :: m -> Void -> Void
    (|*>) _ = id

{- | Product of actions. -}
instance (Action m a , Action m b) => Action m (a, b) where
    (|*>) :: m -> (a, b) -> (a, b)
    (|*>) m (x, y) = (m |*> x, m |*> y)

{- | Coproduct of actions. -}
instance (Action m a , Action m b) => Action m (Either a b) where
    (|*>) :: m -> Either a b -> Either a b
    (|*>) m (Left x) = Left (m |*> x)
    (|*>) m (Right y) = Right (m |*> y)

{- | Pointwise action on action-valued functions. -}
instance Action m b => Action m (a -> b) where
    (|*>) :: m -> (a -> b) -> (a -> b)
    (|*>) m f x = m |*> f x

{- | Lift an action to 'Maybe'. -}
instance Action m a => Action m (Maybe a) where
    (|*>) :: m -> Maybe a -> Maybe a
    (|*>) m = fmap (m |*>)

{- | Lift an action to the pointwise action on lists. -}
instance Action m a => Action m [a] where
    (|*>) :: m -> [a] -> [a]
    (|*>) m = fmap (m |*>)

{- | Lift an action to the pointwise action on 'Seq'. -}
instance Action m a => Action m (Seq a) where 
    (|*>) :: m -> Seq a -> Seq a
    (|*>) m = fmap (m |*>)

{- | Lift an action to the pointwise action on 'Vect.Vector'. -}
instance Action m a => Action m (Vect.Vector a) where
    (|*>) :: m -> Vect.Vector a -> Vect.Vector a
    (|*>) m = fmap (m |*>)

{- | Lift an action to the pointwise action on 'SVect.Vector'. -}
instance Action m a => Action m (SVect.Vector a) where
    (|*>) :: m -> SVect.Vector a -> SVect.Vector a
    (|*>) m = fmap (m |*>)

{- | Lift an action to the pointwise action on 'UVect.Vector'. -}
instance (UVect.Unbox a, Action m a) => Action m (UVect.Vector a) where
    (|*>) :: m -> UVect.Vector a -> UVect.Vector a
    (|*>) m = UVect.map (m |*>)

{- | Lift an action to the pointwise action on 'StVect.Vector'. -}
instance (StVect.Storable a, Action m a) => Action m (StVect.Vector a) where
    (|*>) :: m -> StVect.Vector a -> StVect.Vector a
    (|*>) m = StVect.map (m |*>)

{- | Lift an action to the pointwise action on 'Array.Array'. -}
instance Action m a => Action m (Array.Array i a) where
    (|*>) m = fmap (m |*>)


-- Instances that need UndecidableInstances.
{- | Lift an action on 'Word8' to the pointwise action on 'Bytes.ByteString'. -}
instance (Monoid m, Action m Word8) => Action m Bytes.ByteString where
    (|*>) :: m -> Bytes.ByteString -> Bytes.ByteString
    (|*>) m = Bytes.map (m |*>)

{- | Lift an action on 'Word8' to the pointwise action on 'LBytes.ByteString'. -}
instance (Monoid m, Action m Word8) => Action m LBytes.ByteString where
    (|*>) :: m -> LBytes.ByteString -> LBytes.ByteString
    (|*>) m = LBytes.map (m |*>)

{- | Lift an action on 'Word8' to the pointwise action on 'SBytes.ShortByteString'. -}
instance (Monoid m, Action m Word8) => Action m SBytes.ShortByteString where
    (|*>) :: m -> SBytes.ShortByteString -> SBytes.ShortByteString
    (|*>) m = SBytes.map (m |*>)

{- | Lift an action on 'Char' to the pointwise action on 'Text.Text'. -}
instance (Monoid m, Action m Char) => Action m Text.Text where
    (|*>) :: m -> Text.Text -> Text.Text
    (|*>) m = Text.map (m |*>)

{- | Lift an action on 'Char' to the pointwise action on 'LText.Text'. -}
instance (Monoid m, Action m Char) => Action m LText.Text where
    (|*>) :: m -> LText.Text -> LText.Text
    (|*>) m = LText.map (m |*>)


{- | The monoid morphism induced by a monoid action. -}
transfer :: Action m a => m -> Endo a
transfer m = Endo (m |*>)


{- | The free @m@-action on @a@, isomorphic to @Writer m a@. -}
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


{- | Universal property of the unit of the 'Free' adjunction.

@'free' f :: Free m a -> b@ is the unique equivariant map such that

prop> f = free f . pure
-}
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
map itself is to be equivariant then that leaves \(\int_{M} m = 1\) as the only option, and this
gives the definition of 'extract'.
-}
instance Monoid m => Comonad (Cofree m) where
    {- | The counit of the 'Cofree' adjunction. -}
    extract :: Cofree m a -> a
    extract (Cofree f) = f mempty

    duplicate :: Cofree m a -> Cofree m (Cofree m a)
    duplicate (Cofree f) = Cofree $ \ m -> Cofree $ \ n -> f (m <> n)


{- | The unit of the 'Cofree' adjunction. -}
copure :: Action m a => a -> Cofree m a
copure x = Cofree (|*> x)

{- | Universal property of the counit of the 'Cofree' adjunction.

@'cofree' f :: b -> 'Cofree' m b@ is the unique equivariant map such that:

prop> extract . cofree f = f
-}
cofree :: Action m a => (a -> b) -> a -> Cofree m b
cofree f x = Cofree $ f . (|*> x)


{- | Lift an action to a functor. -}
newtype Lift f a = Lift (f a)
    deriving newtype (Functor, Applicative, Monad, Foldable)

instance (Action m a, Functor f) => Action m (Lift f a) where
    (|*>) :: m -> Lift f a -> Lift f a
    (|*>) m (Lift x) = Lift $ fmap (m |*>) x


{- | Constructing integral values bit by bit. -}
newtype BitArray n = BitArray n
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show, Enum, Num, Real, Integral, Bits, FiniteBits)

{- | Lift an action on 'Bool' to the bitwise action on @'BitArray' w@. -}
instance (Monoid m, Action m Bool, Integral w, FiniteBits w) => Action m (BitArray w) where
    (|*>) :: m -> BitArray w -> BitArray w
    (|*>) m = BitArray . Bits.pack . fmap (m |*>) . Bits.bits


{- | Constructing integral values byte by byte. -}
newtype ByteArray n = ByteArray n
    deriving stock (Eq, Ord, Bounded, Ix)
    deriving newtype (Show, Enum, Num, Real, Integral, Bits, FiniteBits)

{- | Lift an action on 'Word8' to the bytewise action on a finite bits integral. -}
instance (Monoid m, Action m Word8, Integral w, FiniteBits w) => Action m (ByteArray w) where
    (|*>) :: m -> ByteArray w -> ByteArray w
    (|*>) m = ByteArray . Bytes.pack . fmap (m |*>) . Bytes.bytes
