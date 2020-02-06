{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pineappl
  ( Prob(P)
  , FDist(FDist)
  , fdist
  , factor
  , sample
  , hist
  , runFDist
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

newtype FDist a b = FDist { sample :: WriterT (Prob a) (MaybeT []) b }

factor :: RealFrac a => Prob a -> WriterT (Prob a) (MaybeT []) ()
factor = tell

instance (Eq a, RealFrac a, Ord b) => Eq (FDist a b) where
  d1 == d2 = mkHist d1 == mkHist d2 where mkHist = hist . runFDist

fdist :: Ord b => [(b, Prob a)] -> FDist a b
fdist = FDist . WriterT . MaybeT . fmap Just

uniform :: (Num a, Ord b) => [b] -> FDist a b
uniform xs = fdist $ fmap (, P 1) xs

bernoulli :: Num a => Prob a -> FDist a Bool
bernoulli p = fdist [(True, p), (False, 1 - p)]

runFDist :: FDist a b -> [(b, Prob a)]
runFDist = catMaybes . runMaybeT . runWriterT . sample

hist :: (Ord a, RealFrac b) => [(a, b)] -> Map a b
hist ps = fmap (* recip norm) unnormed
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
