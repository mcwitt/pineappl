{-# LANGUAGE TupleSections #-}
import           Test.Hspec

import           Control.Monad                  ( guard )
import           Data.Ratio
import           Pineappl                       ( WrappedBDDist(BDDist)
                                                , Prob(P)
                                                , factor
                                                , bddist
                                                , sample
                                                , hist
                                                , runBDDist
                                                , uniform
                                                , bernoulli
                                                )

main :: IO ()
main = hspec $ do
  describe "DIPPL tutorial milestones" $ do
    let fairCoin = uniform [0, 1]
        coinFlip = sample fairCoin
    it "should get correct result for 3 coin flips"
      $ BDDist ((\a b c -> a + b + c) <$> coinFlip <*> coinFlip <*> coinFlip)
      `shouldBe` bddist
                   [ (0, P (1 % 8))
                   , (1, P (3 % 8))
                   , (2, P (3 % 8))
                   , (3, P (1 % 8))
                   ]

    it "should get correct result for 3 coin flips (monadic)"
      $          BDDist
                   (do
                     a <- coinFlip
                     b <- coinFlip
                     c <- coinFlip
                     return (a + b + c)
                   )
      `shouldBe` bddist
                   [ (0, P (1 % 8))
                   , (1, P (3 % 8))
                   , (2, P (3 % 8))
                   , (3, P (1 % 8))
                   ]

    it "should get correct result for 'funny binomial'"
      $          BDDist
                   (do
                     a <- coinFlip
                     b <- coinFlip
                     c <- coinFlip
                     factor $ P (if a + b > 0 then 1 else 1 % 4)
                     return (a + b + c)
                   )
      `shouldBe` bddist
                   [ (0, P (1 % 32))
                   , (1, P (9 % 32))
                   , (2, P (3 % 8))
                   , (3, P (1 % 8))
                   ]

  describe "Monty Hall" $ do
    it "should get correct answer with the switch"
      $          BDDist
                   (do
                     initialChoice <- sample $ uniform [1 .. 3]
                     revealed      <- case initialChoice of
                       1 -> sample $ uniform [2, 3]
                       2 -> return 3
                       3 -> return 2
                     case (initialChoice, revealed) of
                       (1, 2) -> return 3
                       (1, 3) -> return 2
                       (2, 3) -> return 1
                       (3, 2) -> return 1
                   )
      `shouldBe` bddist [(1, P (1 % 2)), (2, P (1 % 4)), (3, P (1 % 4))]

  describe "Bayes theorem" $ do
    it "should get correct answer for testing problem"
      $ let br  = P (1 % 200)  -- P(+); base rate
            fnr = P (1 % 100)  -- P(-|+); false negative rate
            fpr = P (1 % 100)  -- P(+|-); false positive rate
            pr  = (1 - fnr) * br / ((1 - fnr) * br + fpr * (1 - br))
        in  BDDist
                (do
                  pos     <- sample $ bernoulli br
                  testPos <- sample . bernoulli $ if pos then 1 - fpr else fnr
                  guard testPos
                  return pos
                )
              `shouldBe` bernoulli pr
