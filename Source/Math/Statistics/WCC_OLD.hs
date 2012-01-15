{-# LANGUAGE RecordWildCards,ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Math.Statistics.WCC_OLD(wcc,WCCParams(..)) where
import Math.Statistics(pearson)
import Math.Statistics.WCC.Types
--import Data.List(genericLength)
--pearson x  = (+(-1)) . (*2) . (o/genericLength x) . genericLength . filter (==True) . (zipWith (==) x)
wcc :: Floating a => WCCParams -> [a] -> [a] -> [[a]]
wcc (WCCParams {..}) xs ys = reverse $ wcc' (reverse xs) (reverse ys)
  where  wcc' xs ys | length xsSplit < lagSteps+1 || length ysSplit < lagSteps+1 = []
                    | otherwise = (negLag ++ [zeroLag] ++ posLag) : wcc' (next xs) (next ys) 
          where negLag  = reverse $ pearsonMap ys xsSplit 
                posLag  = pearsonMap xs ysSplit
                zeroLag = pearson (take windowSize xs) (take windowSize ys)
                pearsonMap xs ys = map (pearson (take windowSize xs)) 
                                       (take lagSteps ys)
                next xs = drop windowIncrement xs
                xsSplit = wccSplit' xs
                ysSplit = wccSplit' ys
                -- wccSplit' [] = []
                -- wccSplit' l = tail $ wccSplit windowSize lagIncrement l
                wccSplit' (wccSplit windowSize lagIncrement -> l) | null l = [] 
                                                                  | otherwise = tail l
         

wccSplit :: Int -> Int -> [a] -> [[a]]
wccSplit n _ l | length l < n = []
wccSplit n k l = take n l : wccSplit n k (drop k l)

-- testParams = WCCParams { windowIncrement = 10
--                        , windowSize      = 20
--                        , lagSteps          = 3
--                        , lagIncrement    = 2
--                        }
-- testData :: [Double]
-- testData = concat . replicate 2 $ replicate 15 1 ++ replicate 5 0

testData = concat . replicate 5 $ replicate 5 0 ++ replicate 5 1
testEx = wcc (WCCParams 1 8 5 1) testData testData
