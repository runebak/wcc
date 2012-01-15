module Math.Statistics.WCC.Types where
data WCCParams = WCCParams { windowIncrement :: Int -- ^ Window increment
                           , windowSize      :: Int -- ^ Window size
--                         , maxLag          :: Int -- ^ max lag
                           , lagSteps        :: Int -- ^ max lag
                           , lagIncrement    :: Int -- ^ lag increment
                           } deriving (Show)
