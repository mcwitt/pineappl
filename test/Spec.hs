import           Test.Hspec

import           Data.Bifunctor                 ( second )
import           Data.Complex
import           Data.List                      ( sortBy
                                                , maximumBy
                                                , sortOn
                                                )
import           Data.Ord                       ( compare )
import           Data.Ratio
import           Pineappl                       ( WrappedBDDist(BDDist)
                                                , Prob(P)
                                                , LogProb(LP)
                                                , bddist
                                                , factor
                                                , sample
                                                , condition
                                                , hist
                                                , runBDDist
                                                , uniform
                                                )
import           Pineappl.Quantum               ( observe )

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
    let bernoulli p = bddist [(True, p), (False, 1 - p)]
    it "should get correct answer for testing problem"
      $ let br  = P (1 % 200)  -- P(+); base rate
            fnr = P (1 % 100)  -- P(-|+); false negative rate
            fpr = P (1 % 100)  -- P(+|-); false positive rate
            pr  = (1 - fnr) * br / ((1 - fnr) * br + fpr * (1 - br))
        in  BDDist
                (do
                  pos     <- sample $ bernoulli br
                  testPos <- sample . bernoulli $ if pos then 1 - fpr else fnr
                  condition testPos
                  return pos
                )
              `shouldBe` bernoulli pr

    it "should get correct answer for traffic problem"
      $ let president = bernoulli $ P (1 % 100)
            accident  = bernoulli $ P (1 % 10)
            traffic p a = bernoulli . P $ case (p, a) of
              (False, False) -> 1 % 10
              (False, True ) -> 1 % 2
              (True , False) -> 3 % 5
              (True , True ) -> 9 % 10
            joint = do
              p <- sample president
              a <- sample accident
              t <- sample $ traffic p a
              return (p, a, t)
        in  do
              BDDist (joint >>= \(p, a, t) -> condition t >> return a)
                `shouldBe` bernoulli (P (8 % 23))
              BDDist (joint >>= \(p, a, t) -> condition (t && p) >> return a)
                `shouldBe` bernoulli (P (1 % 7))

  describe "Quantum mechanics" $ do
    it "should get correct answer for double-slit experiment"
      $ let
          ampl y = LP $ exp (i * ds)
           where
            i  = 0 :+ 1
            ds = sqrt $ 1 + fromIntegral y ^ 2
          outcome = BDDist
            (do
              slit <- sample $ bddist [ (y, ampl y) | y <- [-1, 1] ]
              sample $ bddist [ (y, ampl (y - slit)) | y <- [-10 .. 10] ]
            )
          intensity = runBDDist $ observe outcome
          sorted =
            fmap snd
              . sortBy (flip compare)
              . fmap (\(a, b) -> (b, a))
              $ intensity
        in
          {- Looks like:
              -10 | #
              -9 | ####
              -8 | ######
              -7 | ######
              -6 | ###
              -5 | #
              -4 |
              -3 | ###
              -2 | ######
              -1 | #########
               0 | ###########
               1 | #########
               2 | ######
               3 | ###
               4 |
               5 | #
               6 | ###
               7 | ######
               8 | ######
               9 | ####
              10 | # -}
          take 5 sorted `shouldBe` [0, 1, -1, 8, -8]

