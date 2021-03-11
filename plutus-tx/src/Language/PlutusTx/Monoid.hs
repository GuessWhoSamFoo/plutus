{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Language.PlutusTx.Monoid (Monoid (..), mappend, mconcat, Group (..), gsub) where

import           Data.Monoid                 (First (..))
import           Data.Semigroup              (Dual (..), Endo (..))
import qualified Language.PlutusTx.Builtins  as Builtins
import           Language.PlutusTx.Functor   (id)
import           Language.PlutusTx.Semigroup
import           Prelude                     hiding (Monoid (..), Semigroup (..), id, mconcat)

{-# ANN module ("HLint: ignore"::String) #-}

class Semigroup a => Monoid a where
    mempty :: a
    -- mappend and mconcat deliberately omitted, to make this a one-method class which has a
    -- simpler representation

{-# INLINABLE mappend #-}
mappend :: Monoid a => a -> a -> a
mappend = (<>)

{-# INLINABLE mconcat #-}
-- | Fold a list using the monoid.
mconcat :: Monoid a => [a] -> a
mconcat = foldr mappend mempty

instance Monoid Builtins.ByteString where
    {-# INLINABLE mempty #-}
    mempty = Builtins.emptyByteString

instance Monoid Builtins.String where
    {-# INLINABLE mempty #-}
    mempty = Builtins.emptyString

instance Monoid [a] where
    {-# INLINABLE mempty #-}
    mempty = []

instance Semigroup a => Monoid (Maybe a) where
    {-# INLINABLE mempty #-}
    mempty = Nothing

instance Monoid () where
    {-# INLINABLE mempty #-}
    mempty = ()

instance (Monoid a, Monoid b) => Monoid (a, b) where
    {-# INLINABLE mempty #-}
    mempty = (mempty, mempty)

instance Monoid a => Monoid (Dual a) where
    {-# INLINABLE mempty #-}
    mempty = Dual mempty

instance Monoid (Endo a) where
    {-# INLINABLE mempty #-}
    mempty = Endo id

instance Monoid (First a) where
    {-# INLINABLE mempty #-}
    mempty = First Nothing

class Monoid a => Group a where
    inv :: a -> a

{-# INLINABLE gsub #-}
gsub :: Group a => a -> a -> a
gsub x y = x <> inv y
