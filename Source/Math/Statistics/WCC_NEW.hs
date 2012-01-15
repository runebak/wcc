{-# LANGUAGE RecordWildCards,ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Math.Statistics.WCC_NEW(wcc,WCCParams(..)) where
import Math.Statistics(pearson)
import Math.Statistics.WCC.Types
import DebugUtils(dimension)
--import Data.List(genericLength)
--pearson x  = (+(-1)) . (*2) . (o/genericLength x) . genericLength . filter (==True) . (zipWith (==) x)
wcc :: Floating a => WCCParams -> [a] -> [a] -> [[a]]
wcc (WCCParams {..}) xs ys = reverse $ wcc' (lagSplit $ reverse xs) (lagSplit $ reverse ys)
  where  wcc' [] _ = []
         wcc' _ [] = []
         wcc' (xs {- :nextxs -}) (ys {-:nextys-}) | length xsSplit < lagSteps+1 || length ysSplit < lagSteps+1 = []
                                      | otherwise = lag : wcc' (next xs) (next ys) 
          where lag = wccZipper xsSplit ysSplit
                --lag = (negLag ++ [zeroLag] ++ posLag)
                negLag  = reverse $ pearsonMap ys (tail xsSplit) 
                posLag  = map (pearson (take windowSize xs)) (take lagSteps $ tail ysSplit)
                zeroLag = pearson (take windowSize xs) (take windowSize ys)
                pearsonMap xs ys = map (pearson (take windowSize xs)) 
                                       (take lagSteps ys)
                wccSplit =take (lagSteps+1) . splitDrop windowSize lagIncrement
                next = drop windowIncrement
                xsSplit = wccSplit xs
                ysSplit = wccSplit ys
         lagSize = windowSize+lagSteps*lagIncrement
         lagSplit = id -- splitDrop lagSize windowIncrement
wccZipper :: Floating a => [[a]] -> [[a]] -> [a]
wccZipper [] _ = []
wccZipper _ [] = []
wccZipper (x:xs) (y:ys) = map (pearson x) (reverse ys) ++ (pearson x y : map (pearson y) xs)

wccMapper :: WCCParams -> [a] -> [[[a]]]
wccMapper (WCCParams {..}) = map (takeRows . splitDropInf windowSize lagIncrement) 
                             . splitDrop lagSize windowIncrement 
  where lagSize = windowSize+lagSteps*lagIncrement
        takeRows = take (lagSteps+1)

splitDrop :: Int -> Int -> [a] -> [[a]]
splitDrop n = takeWhile ((==n) . length) .: splitDropInf n

splitDropInf :: Int -> Int -> [a] -> [[a]]
splitDropInf n k l = take n l : splitDropInf n k (drop k l)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
infixr 8 .:
