{-# LANGUAGE TypeOperators #-}
module Data.LabeledTree where

import Data.Traversable
import Data.Foldable
import Control.Applicative

data Tree k a = Node  {
      rootLabel :: a, -- ^ label value
      subForest :: Forest k a -- ^ zero or more child trees
} deriving (Show, Eq, Read)

-- | label + value pair
data k ::> a = k ::> a
 deriving (Show, Eq, Read)

type Forest k a = [k ::> Tree k a]

instance Functor ((::>) k) where
    fmap = fmapDefault

instance Foldable ((::>) k) where
    foldMap = foldMapDefault

instance Traversable ((::>) k) where
    traverse f (k ::> v) = (::>) k <$> f v

instance Functor (Tree k) where
    fmap = fmapDefault

instance Foldable (Tree k) where
    foldMap = foldMapDefault

instance Traversable (Tree k) where   
    traverse f (Node l sf) = Node <$> f l <*> (traverse (traverse (traverse f)) sf)
