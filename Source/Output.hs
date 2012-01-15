module Output where
import Data.ByteString.Lazy(ByteString)
import Data.Word(Word8)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.ShowDouble(showDouble)
import Codecs.PngCreate
-- import Data.Colour.Names
-- import Data.Colour(blend)
-- import Data.Colour.SRGB(toSRGB24)
import Data.Colour.RGBSpace(uncurryRGB,RGB(..))
import Data.Colour.RGBSpace.HSL(hsl)
type Color = (Word8, Word8, Word8)
toColor :: Double -> Color
--toColor = undefined
toColor = uncurryRGB (\r g b -> (floor $ 255*r,floor $ 255*g,floor $ 255*b)) . hsl' convert
     where hsl' convert x | x > 1.1 || x < -1.1 = RGB 0 0 0
                          | otherwise = hsl (convert x) 1.0 0.5

-- | Values taken from the suggested 8 colour rainbow, (with slighty more blue)
-- | at http://www.comfsm.fm/~dleeling/cis/hsl_rainbow.html
-- | better than @convert' x = 150*(x+1)@ because we have good colors at nice intervals (cyan at 0.00 etc)
convert :: Double -> Double
convert x | x >=  0.75 = 4*( 1.00-x)*30
          | x >=  0.50 = 4*( 0.75-x)*30 +  30
          | x >=  0.25 = 4*( 0.50-x)*60 +  60
          | x >=  0.00 = 4*( 0.25-x)*60 + 120
          | x >= -0.25 = 4*(     -x)*30 + 180
          | x >= -0.50 = 4*(-0.25-x)*30 + 210
          | x >= -0.75 = 4*(-0.50-x)*30 + 240
          | otherwise  = 4*(-0.75-x)*30 + 270
toPNG :: [[Double]] -> ByteString
toPNG = png . map (map toColor)
toRaw :: [[Double]] -> ByteString
toRaw = B.unlines . map (B.intercalate spacing . map showDouble)
  where spacing = B.pack "  "
        
-- toColor :: Double -> Color
-- toColor d | d<0  = interpolate (between d (-1,0)) blue green 
--           | d>0  = interpolate (between d (0,1)) green red
--           -- | d<(-0.5)  = interpolate (between d (-1,-0.5)) blue green
--           -- | d<(-0.5)  = interpolate (between d (-1,-0.5) blue green
--           | otherwise = (255,255,255)
--   where x = min 255 $ floor $ 255 * abs d
-- between :: Double -> (Double,Double) -> Double
-- between t (r,s) = (t-r)/(s-r)
-- interpolate :: Double -> Color -> Color -> Color 
-- interpolate t d1 d2 = liftV3 round $ (t *: d2) +: ((1-t) *: d1) 
-- liftV3 f (a,b,c) = (f a, f b, f c)
-- (*:) r = liftV3 ((r*) . fromIntegral)
-- (a1,b1,c1) +: (a2,b2,c2) = (a1+a2,b1+b2,c1+c2)
-- red = (255,0,0)
-- green = (0,255,0)
-- blue = (0,0,255)
-- white = (0,0,0)
-- black = (255,255,255)