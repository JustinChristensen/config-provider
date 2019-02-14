module Class where

import Data.Char (toUpper)

data Options = Options {
        formatter :: String -> String
    }

options :: Options
options = Options $ map toUpper

class Show a => Formatter a where
    doFmt :: a -> String
    doFmt = doFmtOptions options

    doFmtOptions :: Options -> a -> String
    doFmtOptions opts = formatter opts . show

