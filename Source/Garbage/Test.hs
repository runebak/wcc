module Test where
import WCC.Utils --(fileToMatrix)
import WCC.Parse --(parseFile)
import Png(png)
import WCC.Types
import WCC
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as B
import System.Process(system)
import Control.Monad(when)
import System.Exit(ExitCode (ExitFailure),exitWith)
import Math.Statistics(pearson)
-- r = replicate 120 (255,0,0)
-- b = replicate 120 (0,255,0)
-- g = replicate 120 (0,0,255)

-- imgData = replicate 40 r ++ replicate 40 b ++ replicate 40 g

-- img = png imgData

-- orgParams = Param { windowIncrement = 20
--                , windowSize      = 120
--                , maxLag          = 400
--                , lagIncrement    = 10
--                }
--testData = concat . replicate 20 $ replicate 15 1 ++ replicate 5 0
testImg = repl (255,0,0) ++ repl (0,255,0)  -- ++ repl (0,0,0) ++ repl (0,0,0)
repl = replicate 20 . replicate 41


testParams' = Param 1 2 2 1
testData' = take 16 . cycle $ [0,1]
paramtoC (Param wInc wMax tSteps tInc) = unwords ["-wInc",show wInc
                                               ,"-wMax",show wMax
                                               ,"-tMax",show (tInc*tSteps)
                                               ,"-tInc",show tInc
                                               ]
--params = Param 20 120 400 10
testFile = "example_dyad_data.txt"
wcc_C = "/home/rune/Dropbox/wcc/c/windcross"
--testImg = repl (255,0,0) ++ repl (0,255,0) ++ repl (255,255,255) ++ repl (0,0,0)
row n = map (pearson $ take 20 dat) $ (take 40 $ wccSplit 20 2 dat)
        where dat = drop (n*2) testData
main = do when (not $ checkMatrix testImg) $ error "wrong Hs matrix"
          B.writeFile "testImg.png" . png $ testImg
colorToGray :: (Int,Int,Int) -> Int
colorToGray (r,g,b) = floor $ 0.3*r'+0.59*g'+0.11*b'
  where r' = fromIntegral r
        g' = fromIntegral g
        b' = fromIntegral b
main' = 
  do putStrLn "writing testData to file"
     writeFile "testData.txt" $ dataToFile testData testData
     let result = wcc testParams testData testData
     putStrLn "writing results to file"
     matrixToFile "testResultHs.txt" result
     putStrLn "checking Results"
     when (not $ checkMatrix result) $ error "wrong Hs matrix"
     putStrLn "Writing image to file"
     B.writeFile "testImgHs.png" $ matrixToPng result
     putStrLn "running C command"
     let c_command = unwords $ [wcc_C
                               ,paramtoC testParams
                               ,"-i testData.txt"
                               ,"-o testResultsC.txt"
                               ]
     putStrLn c_command
     errorCode <- system c_command
     case errorCode of
       ExitFailure x -> putStrLn $ "C_code ended with exit code " ++ show x
       _ -> do putStrLn "writing C results to imagefile"
               c_res <- readMatrix  <$> readFile "testResultsC.txt" 
               when (not $ checkMatrix result) $ error "wrong C matrix"
               B.writeFile "testImgC.png" $ matrixToPng c_res  
        