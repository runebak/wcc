module Utils where
import Control.Applicative((<$>))
import Math.Statistics.WCC(wcc)
import Data.List(intercalate)
import Numeric(showFFloat)
import Codecs.PngCreate
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as B
import Data.List(transpose)
import Parse
type Color = (Int, Int, Int)

matrixToFile :: FilePath -> [[Double]] -> IO ()
matrixToFile f = writeFile f . unlines . map (intercalate "  " . map showFloat)
  where showFloat d | d<0       = d'
                    | otherwise = ' ' : d'
          where d' = showFFloat (Just 8) d "" 
toColor' :: Double -> Color
toColor' _ = (255,0,0)

toColor :: Double -> Color
toColor d | d > 1.1    = (255,255,255)
          | d < -1.1    = (0,0,0)
          | d>0       = (0,x,0) 
          | otherwise = (x,0,0)
  where x = max 255 $ floor $ 255 * abs d
matrixToPng :: [[Double]] -> ByteString
matrixToPng = png . map (map toColor)
dataToFile :: [Double] -> [Double] -> String
dataToFile = unlines .: zipWith (\x y -> unwords [show x,show y])
infixr 8 .:
(.:) = ((.).(.))