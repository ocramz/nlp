{-# language LambdaCase, DeriveFunctor#-}
module Data.Histogram (Histo, histogram, density) where

import Data.Fixed (Fixed, E2, E3, E6)

-- containers
import qualified Data.Map as M (Map, fromList, insert, lookup, empty, alter)

newtype Histo a c = Histo (M.Map a c) deriving (Eq, Show, Functor)

-- -- pinned to Fixed E6 just for display purposes
-- density :: (Ord a, Foldable t) => t a -> Histo a (Fixed E6)
density xs = (recip n *) <$> histogram xs 
  where
    n = fromIntegral $ length xs

histogram :: (Foldable f, Ord a, Num c) => f a -> Histo a c
histogram = foldl insf (Histo M.empty)
  where
    insf = flip insert

insert :: (Ord a, Num c) => a -> Histo a c -> Histo a c
insert k (Histo mm) = Histo $ M.alter insf k mm where
  insf = \ case 
    Nothing -> Just 1
    Just n  -> Just $ n + 1
