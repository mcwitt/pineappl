module StochasticSpec where

import           Test.Hspec

import           Pineappl.Stochastic            ( WrappedBSDist(BSDist)
                                                , sample
                                                , samples
                                                , stdUniform
                                                )
import           System.IO.Unsafe

spec :: Spec
spec = describe "Sampling" $ do
  let mean xs = sum xs / fromIntegral (length xs)
  it "should approximate π by sampling"
    $ (unsafePerformIO . fmap ((4 *) . mean) . samples 100 $ BSDist
        (do
          x <- sample (stdUniform :: WrappedBSDist Double)
          y <- sample stdUniform
          return $ if x ^ 2 + y ^ 2 < 1 then 1 else 0
        )
      )
    `shouldSatisfy` (\x -> let re = (x - pi) / pi in abs re < 0.1)
