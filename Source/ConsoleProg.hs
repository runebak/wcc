module Main where
import Math.Statistics.WCC(wcc)
import DebugPrint(debugPrint)
import Output(toPNG,toRaw)
import Parse(parseFile)
import CommandLine(withArgs,OutputFormat(..))
--import Data.ByteString.Lazy.Progress
--import Control.Applicative((<*>))
--import Control.Monad(liftM2,(>=>))
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
--import System.IO(openBinaryFile,hFileSize,IOMode(ReadMode))
main :: IO ()
main = withArgs main'
main' infile outfile debug outputFormat params = 
  B.readFile infile >>= eatData debug . computeWCC params 
    where eatData True  = debugPrint infile outfile params
          eatData False = B.writeFile outfile . toOutput outputFormat
computeWCC p = uncurry (wcc p) . parseFile
toOutput :: OutputFormat -> [[Double]] -> ByteString
toOutput PNG = toPNG
toOutput RAW = toRaw


-- the tracker wasn't very useful, so I commented it out

-- readFileWithTracker :: FilePath -> IO B.ByteString
-- readFileWithTracker = flip openBinaryFile ReadMode >=> hFileSize &=& B.hGetContents 
--                                                    >=> uncurry tracker
-- tracker size = trackProgressWithChunkSize (fromIntegral $ size `div` 100) 
--                                           (const $ const $ putStr ".") 
-- infixr 3 &=& -- (&&&) for monads
-- f &=& g = \a -> liftM2 (,) (f a) (g a) 

--tracker = undefined
--readFileWithTracker f = B.readFile f >>= tracker
--tracker = trackProgressWithChunkSize 8 (const $ const $ putStr ".")
-- tracker _ = trackProgressString "Bytes read: %B, rate %R" Nothing putStrLn
--tracker = trackProgressStringWithChunkSize "." 8 Nothing putStr
--tracker size = trackProgressString "Time left: %T" -- , procent done %p" 
--                                   (Just . fromIntegral $ size) 
--                                   putStrLn
-- progressString = "Bytes read: %%B, current rate: %R"
-- progressPrinter = putStrLn
-- tracker = trackProgressString progressString Nothing progressPrinter


-- computeWCC outputFormat p = showData outputFormat . uncurry (wcc p) . parseFile
--   where showData :: OutputFormat -> [[Double]] -> B.ByteString
--         showData PNG  = png . map (map toColor)
--         showData RAW  = B.unlines . map (B.intercalate spacing . map showFloat)
--         spacing = B.pack "  "


-- showParam infile outfile outputPNG p = 
--   do putStrLn $ "infile: " ++ infile
--      putStrLn $ "outfile: " ++ outfile
--      putStrLn $ if outputPNG then "output: PNG" else "output: RAW"
--      putStrLn $ "Params: " ++ show p