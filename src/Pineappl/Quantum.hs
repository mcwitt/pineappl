module Pineappl.Quantum where

import           Data.Bifunctor
import           Data.Complex
import qualified Data.Map                      as M
import           Pineappl                       ( Prob(P)
                                                , LogProb(LP)
                                                )
import           Pineappl.Deterministic         ( runBDDist
                                                , bddist
                                                , hist
                                                , WrappedBDDist
                                                )

fromAmplitude
  :: (RealFloat a, Ord b)
  => WrappedBDDist (LogProb (Complex a)) b
  -> WrappedBDDist (Prob a) b
fromAmplitude =
  bddist
    . fmap (second (\(LP psi) -> P (magnitude psi ^ 2)))
    . M.toList
    . hist
    . runBDDist
