{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Pineappl.Deterministic
  ( BDDist
  , WrappedBDDist(BDDist)
  , bddist
  , factor
  , sample
  , condition
  , hist
  , runBDDist
  , uniform
  )
where

import           Pineappl                       ( Prob(P) )

import           Control.Monad.Writer
import           Control.Monad.Trans.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           Text.Printf

type BDDist a b = WriterT a (MaybeT []) b

newtype WrappedBDDist a b = BDDist { unwrapBDDist :: BDDist a b }

-- | Alias for 'unwrapBDDist'
sample :: WrappedBDDist a b -> BDDist a b
sample = unwrapBDDist

-- | Alias for 'tell'
factor :: Monoid a => Num a => a -> BDDist a ()
factor = tell

-- | Alias for 'guard'
condition :: Monoid a => Bool -> BDDist a ()
condition = guard

instance (Eq a, Fractional a, Ord b) => Eq (WrappedBDDist a b) where
  d1 == d2 = mkHist d1 == mkHist d2 where mkHist = hist . runBDDist

bddist :: Ord b => [(b, a)] -> WrappedBDDist a b
bddist = BDDist . WriterT . MaybeT . fmap Just

uniform :: (Monoid a, Ord b) => [b] -> WrappedBDDist a b
uniform xs = bddist $ fmap (, mempty) xs

runBDDist :: WrappedBDDist a b -> [(b, a)]
runBDDist = catMaybes . runMaybeT . runWriterT . unwrapBDDist

hist :: (Fractional a, Ord b) => [(b, a)] -> Map b a
hist ps = fmap (* recip norm) unnormed
 where
  unnormed = M.fromListWith (+) ps
  norm     = sum $ M.elems unnormed

instance (RealFrac a, Ord b, Show b) => Show (WrappedBDDist (Prob a) b) where
  show =
    unlines
      . fmap
          (\(x, P p) ->
            printf "%8s" (show x) ++ " | " ++ replicate (floor (100 * p)) '#'
          )
      . M.assocs
      . hist
      . runBDDist
