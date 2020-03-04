{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Pineappl.Stochastic
  ( WrappedBSDist(BSDist)
  , sample
  , stdUniform
  , uniform
  , choice
  , bernoulli
  , multinomial
  , samples
  )
where

import           Pineappl                       ( Prob(P) )

import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class      ( lift )
import           Data.Either
import           Data.List.NonEmpty            as N
import           Data.Maybe
import           System.Random

type BSDist a = MaybeT IO a

newtype WrappedBSDist a = BSDist { unwrapBSDist :: BSDist a } deriving (Functor, Applicative, Monad)

sample = unwrapBSDist

stdUniform :: (Random p, RealFloat p) => WrappedBSDist p
stdUniform = BSDist $ lift randomIO

uniform :: (Random p, RealFloat p) => p -> p -> WrappedBSDist p
uniform l u = (* (u - l)) . (+ l) <$> stdUniform

choice
  :: (Random p, RealFloat p) => a -> b -> Prob p -> WrappedBSDist (Either a b)
choice a b (P p) = (\r -> if r < p then Left a else Right b) <$> stdUniform

bernoulli :: Prob Double -> WrappedBSDist Bool
bernoulli = fmap isLeft . choice () ()

-- | Naive O(n^2) implementation
multinomial
  :: (Random p, RealFloat p) => N.NonEmpty (Prob p, a) -> WrappedBSDist a
multinomial ((p, a) :| xs) = case N.nonEmpty xs of
  Just rs ->
    let q = p / (p + (sum . fmap fst) xs)
    in  choice a (multinomial rs) q >>= \case
          Left  x -> return x
          Right x -> x
  Nothing -> return a

samples :: Int -> WrappedBSDist a -> IO [a]
samples n = fmap catMaybes . replicateM n . runMaybeT . unwrapBSDist
