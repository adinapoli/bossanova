module Utils where

import Data.Word
import Data.Time.Clock.POSIX (getPOSIXTime)

------------------------------------------------------------------------------
milliTime :: IO Word64
milliTime = do
    seconds <- realToFrac `fmap` getPOSIXTime :: IO Double
    return $ round $ seconds * 1e3
