module Pineappl.Quantum where

import           Data.Bifunctor
import           Data.Complex
import           Pineappl                       ( runBDDist
                                                , bddist
                                                , Prob(P)
                                                , LogProb(LP)
                                                , WrappedBDDist
                                                )

observe
  :: (RealFloat a, Ord b)
  => WrappedBDDist (LogProb (Complex a)) b
  -> WrappedBDDist (Prob a) b
observe =
  bddist . fmap (second (\(LP psi) -> P (magnitude psi ^ 2))) . runBDDist
