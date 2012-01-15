{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, RecordWildCards#-}
module CommandLine(withArgs,OutputFormat(..)) where
import System.Console.CmdLib
import qualified Math.Statistics.WCC as T
data OutputFormat = PNG | RAW deriving (Eq,Data,Typeable,Show)
data Opts = Opts { windowIncrement :: Int
                 , windowSize      :: Int
--                 , maxLag          :: Int
                 , lagSteps        :: Int
                 , lagIncrement    :: Int
                 , infile          :: FilePath
                 , outfile         :: FilePath
                 , outputFormat    :: OutputFormat
                 , debug           :: Bool
                 } deriving (Eq,Data,Typeable,Show)

instance Attributes Opts where
  attributes _ = group "Options" 
    [ windowIncrement %> [Help "Window increment", ArgHelp "INT"]
    , windowSize      %> [Help "The window size",  ArgHelp "SIZE"]
--    , maxLag          %> [Help "The maximum lag, should be divisible by lagIncrement"]
    , lagSteps        %> [Help "Number of lag-steps"]
    , lagIncrement    %> [Help "lag increment"]
    , outputFormat    %> [Help "RAW will output the correlation matrix, PNG a png image", Default PNG, ArgHelp "RAW|PNG"]
    , infile          %> [Help "The input filename. Two columns of numbers (lines starting with '#' is ignored)"
                         ,ArgHelp "FILE"]
    , outfile         %> [Help "The output filename", ArgHelp "FILE"]
    , debug           %> [Help "Print debug information", Default False]
    ]
  readFlag _ = readCommon <+< (readOutputFormat) 
    where readOutputFormat "PNG" = PNG
          readOutputFormat "RAW" = RAW
          readOutputFormat x = error $ "unknown output format: " ++ x
instance RecordCommand Opts where
  mode_summary _ = "Compute the windowed cross correlation."
  run' _ = error "run' er undefined"


-- add this to remove the warning about uninitialized fields.
-- they isn't used though
-- defaultOpts = Opts { windowIncrement = ns "windowIncrement"
--                    , windowSize      = ns "windowSize"
--                    , maxLag          = ns "maxLag"
--                    , lagIncrement    = ns "lagIncrement"
--                    , infile          = ns "input filename"
--                    , outfile         = ns "output filename"
--                    , outputFormat    = ns "output format"
--                    } 
--   where ns x = error $ "no " ++x++ " specified"
--withArgs f = getArgs >>= executeR defaultOpts >>= return . parseOpts >>= \(i,o,out,p) -> f i o out p 
withArgs :: (FilePath -> FilePath -> Bool -> OutputFormat -> T.WCCParams -> IO ()) -> IO ()
withArgs f = getArgs >>= executeR Opts {} >>= return . parseOpts >>= \(i,o,debug,out,p) -> f i o debug out p 
--withArgs f = getArgs >>= dispatchR [] >>= f 
parseOpts :: Opts -> (FilePath,FilePath,Bool,OutputFormat,T.WCCParams)
parseOpts (Opts {..}) | windowIncrement < 1 = bto "windowIncrement"
                      | windowSize      < 1 = bto "windowSize"                      
--                      | maxLag          < 1 = bto "maxLag"
                      | lagSteps        < 1 = bto "lagSteps"
                      | lagIncrement    < 1 = bto "lagIncrement"    
--                      | lagErr /= 0         = error $ "maxLag not divisible by lagIncrement" 
                      | infile  == ""       = nfg "in"                        
                      | outfile == ""       = nfg "out"
                      | otherwise = (infile,outfile,debug,outputFormat,T.WCCParams {..})
  where bto   = error . (++" must be bigger than one.")
        nfg x = error $ "no " ++ x ++ "file given." 
--        (lagSteps,lagErr) = maxLag `divMod` lagIncrement