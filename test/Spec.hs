import           Test.Hspec

import           Data.Ratio
import           Pineappl                       ( FDist(FDist)
                                                , Prob(P)
                                                , factor
                                                , fdist
                                                , fsample
                                                , hist
                                                , runFDist
                                                )

main :: IO ()
main = hspec $ do
  describe "DIPPL tutorial milestones" $ do
    let fairCoin = fdist [(0, P (1 % 2)), (1, P (1 % 2))]
        coinFlip = fsample fairCoin
    it "should get correct result for 3 coin flips"
      $ FDist ((\a b c -> a + b + c) <$> coinFlip <*> coinFlip <*> coinFlip)
      `shouldBe` fdist
                   [ (0, P (1 % 8))
                   , (1, P (3 % 8))
                   , (2, P (3 % 8))
                   , (3, P (1 % 8))
                   ]

    it "should get correct result for 3 coin flips (monadic)"
      $          FDist
                   (do
                     a <- coinFlip
                     b <- coinFlip
                     c <- coinFlip
                     return (a + b + c)
                   )
      `shouldBe` fdist
                   [ (0, P (1 % 8))
                   , (1, P (3 % 8))
                   , (2, P (3 % 8))
                   , (3, P (1 % 8))
                   ]

    it "should get correct result for 'funny binomial'"
      $          FDist
                   (do
                     a <- coinFlip
                     b <- coinFlip
                     c <- coinFlip
                     factor $ P (if a + b > 0 then 1 else 0.25)
                     return (a + b + c)
                   )
      `shouldBe` fdist
                   [ (0, P (1 % 32))
                   , (1, P (9 % 32))
                   , (2, P (3 % 8))
                   , (3, P (1 % 8))
                   ]

