module VabalError where

import Control.Exception

newtype VabalError = VabalError String
                deriving(Eq, Show)

instance Exception VabalError where

throwVabalError :: String -> IO a
throwVabalError = throwIO . VabalError
