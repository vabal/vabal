module VabalContext where

import GhcDatabase

data VabalContext = VabalContext
                  { availableGhcs   :: GhcDatabase
                  , allGhcInfo      :: GhcDatabase
                  , alwaysNewestGhc :: Bool
                  }
