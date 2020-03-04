{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pineappl
  ( Prob(P)
  , LogProb(LP)
  )
where

newtype Prob a = P a deriving (Fractional, Num, Ord, Eq, Show)

instance Num a => Semigroup (Prob a) where
  (<>) = (*)

instance Num a => Monoid (Prob a) where
  mempty = 1

newtype LogProb a = LP a deriving (Fractional, Num, Eq, Show)

instance Num a => Semigroup (LogProb a) where
  (<>) = (+)

instance Num a => Monoid (LogProb a) where
  mempty = 0
