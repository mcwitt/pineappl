{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pineappl.Rand where

import           Pineappl                       ( Prob(P) )

import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.List.NonEmpty            as N
import           System.Random

newtype Rand a = Rand { sample :: IO a } deriving (Functor, Applicative, Monad)

choice :: (Random p, RealFloat p) => a -> b -> Prob p -> Rand (Either a b)
choice a b (P p) =
  Rand $ randomIO >>= \r -> return $ if r < p then Left a else Right b

-- | Naive O(n^2) implementation
brdist :: (Random p, RealFloat p) => N.NonEmpty (Prob p, a) -> Rand a
brdist ((p, a) :| xs) = case N.nonEmpty xs of
  Just rs ->
    let q = p / (p + (sum . fmap fst) xs)
    in  choice a (brdist rs) q >>= \case
          Left  x -> return x
          Right x -> x
  Nothing -> return a

bernoulli :: Prob Double -> Rand Bool
bernoulli = fmap isLeft . choice () ()

samples :: Int -> Rand a -> IO [a]
samples n = sample . replicateM n
