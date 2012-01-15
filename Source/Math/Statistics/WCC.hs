{-# LANGUAGE RecordWildCards, ExplicitForAll #-}
{-# OPTIONS_GHC -fspec-constr-count=16 #-}
module Math.Statistics.WCC(wcc,WCCParams(..),wccMapper) where
import Math.Statistics(pearson)
import Math.Statistics.WCC.Types
-- import Test.QuickCheck(quickCheck)
-- import Test.QuickCheck.Property((==>))

wcc,wcc' :: WCCParams -> [Double] -> [Double] -> [[Double]]
wcc params xs ys = reverse . (map reverse) $ wcc' params (reverse xs) (reverse ys)
wcc' params xs ys = zipWith wccZipper (mapper xs) (mapper ys)
 where mapper = wccMapper params

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
infixr 8 .:
splitDropSafe1,splitDropSafe2,splitDropSafe :: Int -> Int -> [a] -> [[a]]
splitDropSafe1 n = takeWhile ((==n) . length) .: splitDrop n
splitDropSafe2 n k l = take size $ splitDrop n k l
  where size = 1+(length l - n) `div` k
splitDropSafe = splitDropSafe1

wccZipper :: [[Double]] -> [[Double]] -> [Double]
wccZipper [] _ = []
wccZipper _ [] = []
wccZipper (x:xs) (y:ys) = map (pearson x) (reverse ys) ++ (pearson x y : map (pearson y) xs)
wccMapper :: WCCParams -> [a] -> [[[a]]]
wccMapper (WCCParams {..}) = map (takeRows . splitDrop windowSize lagIncrement) 
                             . splitDropSafe lagSize windowIncrement 
  where lagSize = windowSize+lagSteps*lagIncrement
        takeRows = take (lagSteps+1)

-- wccFolder :: [[[Double]]] -> [[Double]]
-- wccFolder d = zipWith wccZipper d d 

splitDrop :: Int -> Int -> [a] -> [[a]]
splitDrop n k | k<=0 = error "SplitDrop: increment must be positive!"
              | otherwise =  splitDrop'
  where splitDrop' [] = []
        splitDrop' l = take n l : splitDrop' (drop k l)         


