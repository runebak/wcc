

data IHDR = 
   IHDR { ihdr_width            :: Word32
       , ihdr_height            :: Word32
       , ihdr_bitDepth          :: Word8
       , ihdr_colorType         :: Word8
       , ihdr_compressionMethod :: Word8
       , ihdr_filterMethod      :: Word8
       , ihdr_interlaceMethod   :: Word8
       }
--  | IDAT {idat_data :: B.ByteString}
--  | IEND
data ColorType = Color0 [[Word8]] | Color2 [[(Word8,Word8,Word8)]]
                                            
scanline (Color0 dat) = B.pack $ 0 : (concat dat)
scanline (Color2 dat) = B.pack $ 0 : (concatMap (\(r,g,b) -> [r,g,b]) $ concat dat)

toChunk (IHDR width height bitDepth colorType compressionMethod filterMethod interlaceMethod) = 
  chunk iHDR $ B.concat [ be32 width
                        , be32 height
                        , be8 bitDepth
                        , be8 colorType
                        , be8 compressionMethod
                        , be8 filterMethod
                        , be8 interlaceMethod ]
ihdr_idat dat = B.concat $ hdr : concat [ihdr, imgdat, iend]
  where width = fromIntegral $ length (head dat)
        height = fromIntegral $ length dat
        imgdat = chunk iDAT (Z.compress imagedata)
        imagedata = B.concat $ map scanline dat
        iend = chunk iEND B.empty
        ihdr = (case dat of (Color0 _) -> ihdr_grey 
                            (Color2 _) -> ihdr_color
               ) width height

ihdr_grey  w h = IHDR w h 8 0 0 0 0
ihdr_color w h = IHDR w h 8 2 0 0 0
