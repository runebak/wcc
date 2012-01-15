{-# LANGUAGE ViewPatterns #-}
module DebugPrint where
import Parse
import Math.Statistics.WCC
import Output(toRaw,toPNG)
import Control.Applicative((<$>))
import System.Process(system)
import Control.Monad(when)
import System.Exit(ExitCode (ExitFailure))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8(ByteString)
import DebugUtils
debugPrint :: String -> String -> WCCParams -> [[Double]] -> IO ()
debugPrint infile outfile params rawData= 
  do when (not $ checkMatrix rawData) $ putStrLn "rawData-matrix uneven"
     putStrLn "Writing rawData"
     B.writeFile (outfile++".raw") $ toRaw rawData
     putStrLn "Writing PNG image"
     B.writeFile (outfile++".png") $ toPNG rawData  
     putStrLn "Running C-program"
     cOutput <- runc
     case cOutput of
       Left x -> putStrLn x
       Right cMatrixRaw -> 
         do when (not $ checkMatrix cMatrixRaw) $ putStrLn "C-matrix uneven"
            let cDim = dimension cMatrixRaw
                hDim = dimension rawData
            cMatrix <- if (fst hDim == fst cDim-1 && snd hDim == snd cDim)
                       then do putStrLn "C-output has one extra columns, we'll skip it"
                               return $ drop 1 cMatrixRaw
                       else return cMatrixRaw
            when (not $ sameDimensions rawData cMatrix) ( 
              do putStrLn $ "Output matrix dimension mismatch, Haskell: "
                            ++ show (dimension rawData)
                            ++" But C: " ++ show (dimension cMatrix)
              )
            if (cMatrix `isSimilarTo` rawData )
              then putStrLn "C agrees with Haskell" 
              else putStrLn "C and Haskell-output differ"  
            putStrLn "Converting C-output to PNG"
            B.writeFile (outfile++".fromc.png") (toPNG cMatrix)
    where cOutfileRaw = outfile++".fromc.raw" 
          c_command = unwords $ [wcc_C
                                ,paramtoC params
                                ,"-i",infile
                                ,"-o",cOutfileRaw
                                ]
          runc = 
             do errorCode <- system c_command
                case errorCode of
                  ExitFailure x -> return . Left $ "Unable to test against C code."
                                               ++"C_code ended with exit code " ++ show x
                  _ -> Right . readMatrix  <$> B.readFile cOutfileRaw