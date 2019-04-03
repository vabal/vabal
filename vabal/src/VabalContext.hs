module VabalContext where

import qualified Data.Set as S
import           Distribution.Types.Version
import           GhcDatabase

data VabalContext = VabalContext
                  { availableGhcs :: S.Set Version
                  , ghcDatabase   :: GhcDatabase
                  }
