{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fspec-constr-count=16 #-}
module Parse where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8(ByteString)
import Control.Applicative((<$>))
import Data.ByteString.Lex.Lazy.Double(readDouble)
parseFile :: ByteString -> ([Double],[Double])
parseFile = unzip . map (readRow . B.words) . filter (not . isComment) .  B.lines
readRow :: [ByteString] -> (Double, Double)
readRow (map maybeReadDouble-> [Just x,Just y]) | True = (x,y)
readRow r = error $ "Ill-formed row: " ++ (B.unpack . B.unwords $ r)
maybeReadDouble :: ByteString -> Maybe Double
maybeReadDouble (readDouble -> Just (x,xs)) | B.null xs  = Just x
maybeReadDouble x | Just ('-',xs) <- B.uncons x = negate <$> maybeReadDouble xs
maybeReadDouble x | Just ('.',_) <- B.uncons x = maybeReadDouble $ B.cons '0' x
maybeReadDouble x | bisNaN x = Just nan 
maybeReadDouble _ = Nothing
nan :: Double
nan = 0/0
bisNaN :: ByteString -> Bool
bisNaN x = x == bnan || x == bNaN
bnan,bNaN :: ByteString
bnan  = B.pack  "nan"
bNaN  = B.pack  "NaN"
isComment :: ByteString -> Bool
isComment (B.uncons -> Just(x,_)) | isCommentStart x = True
isComment _ = False
isCommentStart :: Char -> Bool
isCommentStart x = x == '#' || x == '\r' -- last is a hack to strip empty lines 
                                         -- with windows lineendingsp