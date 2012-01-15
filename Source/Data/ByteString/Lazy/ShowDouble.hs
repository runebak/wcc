{-# LANGUAGE ForeignFunctionInterface #-}
module Data.ByteString.Lazy.ShowDouble(showDouble) where
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Internal       as B
import qualified Data.ByteString.Lazy.Char8 as L
import Foreign
import Foreign.C.Types
------------------------------------------------------------------------
--
-- show a Double into a lazy bytestring
--
 
showDouble :: Double -> ByteString
showDouble d = L.fromChunks . return . unsafePerformIO . B.createAndTrim lim $  \p ->
    B.useAsCString fmt $ \cfmt -> do
        n <- c_printf_double (castPtr p) (fromIntegral lim) cfmt (realToFrac d)
        return (min lim (fromIntegral n)) -- snprintf might truncate
  where
    lim = 100 -- n.b.
    fmt = B.pack "%f"
 
foreign import ccall unsafe "static stdio.h snprintf" 
    c_printf_double :: Ptr CChar -> CSize -> Ptr CChar -> CDouble -> IO CInt