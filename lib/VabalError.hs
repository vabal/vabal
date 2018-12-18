module VabalError where

import Control.Exception

newtype VabalError = VabalError String
                deriving(Eq, Show)

instance Exception VabalError where

throwVabalErrorIO :: String -> IO a
throwVabalErrorIO = throwIO . VabalError

throwVabalError :: String -> a
throwVabalError = throw . VabalError
