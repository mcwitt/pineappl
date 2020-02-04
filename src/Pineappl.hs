{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pineappl
  ( Prob(P)
  , FDist(FDist)
  , fdist
  , factor
  , fsample
  , hist
  , runFDist
  )
where

import           Control.Monad.Writer
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Text.Printf

newtype Prob a = P a deriving (Ord, Real, Fractional, RealFrac, Num, Eq, Show)

instance Num a => Semigroup (Prob a) where
  P p <> P q = P (p * q)

instance Num a => Monoid (Prob a) where
  mempty = P 1

newtype FDist a b = FDist { fsample :: WriterT (Prob a) [] b }

factor :: RealFrac a => Prob a -> WriterT (Prob a) [] ()
factor = tell

instance (Eq a, RealFrac a, Ord b) => Eq (FDist a b) where
  d1 == d2 = fn d1 == fn d2 where fn = hist . runFDist

fdist :: Ord b => [(b, Prob a)] -> FDist a b
fdist = FDist . WriterT

runFDist :: FDist a b -> [(b, Prob a)]
runFDist (FDist w) = runWriterT w

hist :: (Ord a, RealFrac b) => [(a, b)] -> Map a b
hist ps = fmap (/ norm) unnormed
 where
  unnormed = M.fromListWith (+) ps
  norm     = sum $ M.elems unnormed

instance (RealFrac a, Ord b, Show b) => Show (FDist a b) where
  show =
    unlines
      . fmap
          (\(x, P p) ->
            printf "%8s" (show x) ++ " | " ++ replicate (floor (100 * p)) '#'
          )
      . M.assocs
      . hist
      . runFDist
