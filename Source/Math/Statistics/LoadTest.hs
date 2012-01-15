module LoadTest where
import qualified Codec.Image.PNG as PNGL
--import Data.Array.Storable
import Data.Array.MArray
import Test(testImg)
import GHC.Word(Word8)
import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.Zlib as Z
import Data.Maybe(fromJust)
import Png
import Text.Parsec.Prim
splitIn _ [] = []
splitIn n l = let (x,l') = splitAt n l in x : splitIn n l' 
imgDataOrg = map fromIntegral . concat . map (\(r,g,b) -> [r,g,b]) . concat $ testImg :: [Word8]
imgData' = PNGL.loadPNGFile "testImg.png" >>= \(Right img) -> getElems . PNGL.imageData $ img

getImageData dat = do img <- PNGL.toPngImage . right . runP PNGL.pngFile () "pngImage" $ dat
                      getElems . PNGL.imageData . right $ img
parseWith p = right . runP p () "prut"
width=3*3
right (Right a) = a
extractDataChunk = B.unpack . Z.decompress . PNGL.rawPngChunk_data . right .  runP PNGL.rawPngChunk () "data"
idhrChunk = undefined
datachunk dat = B.concat $ chunk iDAT (Z.compress imagedata)
  where imagedata = B.concat $ map scanline dat

g = B.concat . map (snd . fromJust. B.uncons) . chop . Z.decompress . Z.compress . B.concat . map scanline
f = concat . map (\(r,g,b) -> [r,g,b]) . concat
chop b
  | B.null b   = []
  | otherwise   = let (sl,rest) = B.splitAt slWidth b
                  in sl : chop rest
  where slWidth = width+1

