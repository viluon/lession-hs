{-# LANGUAGE NumericUnderscores #-}
module Ep00.MainSolution
where

-- extras

type CeléČíslo = Int

-- >>> logBase 2 (fromIntegral (maxBound :: CeléČíslo))

recipFacts' :: [Double]
recipFacts' = map (\i -> 1 / fact i) [0..]
  where
    fact :: Int -> Double
    fact 0 = 1
    fact n = fromIntegral n * fact (n - 1)

fact' :: Int -> Double
fact' n = fromIntegral $ product [1..n]
-- >>> fact' 0

-- >>> take 5 $ map (1 /) recipFacts'

-- >>> sum $ takeWhile (> 0) recipFacts'

-- >>> exp 1
