{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pineappl
  ( Prob(P)
  , WrappedBDDist(BDDist)
  , bddist
  , factor
  , sample
  , hist
  , runBDDist
  , uniform
  , bernoulli
  )
where

import           Control.Monad.Writer
import           Control.Monad.Trans.Maybe
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           Data.Ratio                     ( (%) )
import           Text.Printf

newtype Prob a = P a deriving (Ord, Real, Fractional, RealFrac, Num, Eq, Show)

instance Num a => Semigroup (Prob a) where
  (<>) = (*)

instance Num a => Monoid (Prob a) where
  mempty = 1

type BDDist a b = WriterT (Prob a) (MaybeT []) b

newtype WrappedBDDist a b = BDDist { unwrapBDDist :: BDDist a b }

-- | Alias for 'unwrapBDDist'
sample :: WrappedBDDist a b -> BDDist a b
sample = unwrapBDDist

-- | Alias for 'tell'
factor :: Num a => Prob a -> BDDist a ()
factor = tell

instance (Eq a, RealFrac a, Ord b) => Eq (WrappedBDDist a b) where
  d1 == d2 = mkHist d1 == mkHist d2 where mkHist = hist . runBDDist

bddist :: Ord b => [(b, Prob a)] -> WrappedBDDist a b
bddist = BDDist . WriterT . MaybeT . fmap Just

uniform :: (Num a, Ord b) => [b] -> WrappedBDDist a b
uniform xs = bddist $ fmap (, P 1) xs

bernoulli :: Num a => Prob a -> WrappedBDDist a Bool
bernoulli p = bddist [(True, p), (False, 1 - p)]

runBDDist :: WrappedBDDist a b -> [(b, Prob a)]
runBDDist = catMaybes . runMaybeT . runWriterT . unwrapBDDist

hist :: (Ord a, RealFrac b) => [(a, b)] -> Map a b
hist ps = fmap (* recip norm) unnormed
 where
  unnormed = M.fromListWith (+) ps
  norm     = sum $ M.elems unnormed

instance (RealFrac a, Ord b, Show b) => Show (WrappedBDDist a b) where
  show =
    unlines
      . fmap
          (\(x, P p) ->
            printf "%8s" (show x) ++ " | " ++ replicate (floor (100 * p)) '#'
          )
      . M.assocs
      . hist
      . runBDDist
