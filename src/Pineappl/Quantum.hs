module Pineappl.Quantum where

import           Data.Bifunctor
import           Data.Complex
import qualified Data.Map                      as M
import           Pineappl                       ( runBDDist
                                                , bddist
                                                , hist
                                                , Prob(P)
                                                , LogProb(LP)
                                                , WrappedBDDist
                                                )

observe
  :: (RealFloat a, Ord b)
  => WrappedBDDist (LogProb (Complex a)) b
  -> WrappedBDDist (Prob a) b
observe =
  bddist
    . fmap (second (\(LP psi) -> P (magnitude psi ^ 2)))
    . M.toList
    . hist
    . runBDDist
