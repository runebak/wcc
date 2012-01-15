{-# LANGUAGE ViewPatterns #-}
module DebugUtils where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8(ByteString)
import System.Process(system)
import System.Exit(ExitCode (ExitFailure))
import Data.ByteString.Lazy.ShowDouble(showDouble)
import Control.Applicative
import Parse(maybeReadDouble)
import Math.Statistics.WCC.Types
readMatrix :: ByteString -> [[Double]]
readMatrix = map (map readDouble . B.words)  . B.lines
  where readDouble (maybeReadDouble -> Just d) = d
        readDouble _ = error "error in C file"

dataToFile :: [Double] -> [Double] -> ByteString
dataToFile = B.unlines .: zipWith (\x y -> B.unwords [showDouble x,showDouble y])
(.:) = (.).(.)
infixr 8 .:
epsilon :: Double
epsilon = 0.0001
checkMatrix :: [[a]] -> Bool
checkMatrix l = null l || all (==x) xs
  where (x:xs) = map length l
isSimilarTo :: [[Double]] -> [[Double]] -> Bool
isSimilarTo = and . concat .: zipWith (zipWith (\x y -> (isNaN x && isNaN y) || abs (x-y) < epsilon))
sameDimensions :: [[a]] -> [[a1]] -> Bool
sameDimensions x y = (null x && null y ) || dimension x == dimension y
dimension :: [[a]] -> (Int, Int)
dimension x = (length x,length (head x))


wccC = wccC' "testData.in" "testData.out"
wccC' infile outfile p d1 d2 = 
  do B.writeFile infile $ dataToFile d1 d2
     runc p infile outfile
runc params infile outfile = 
  do errorCode <- system . unwords $ [wcc_C,paramtoC params,"-i",infile,"-o",outfile]
     case errorCode of
       ExitFailure x -> return . Left $ "Unable to test against C code."
                        ++"C_code ended with exit code " ++ show x
       _             -> Right . readMatrix  <$> B.readFile outfile
wcc_C = "/home/rune/Dropbox/wcc/c/windcross"
paramtoC :: WCCParams -> String
paramtoC (WCCParams wInc wMax tSteps tInc) = 
  unwords ["-wInc",show wInc
          ,"-wMax",show wMax
          ,"-tMax",show (tInc*tSteps)
          ,"-tInc",show tInc
          ]
