module FlagsUtils where

import Distribution.Types.GenericPackageDescription
import VabalError

makeCabalArguments :: FlagAssignment -> [String]
makeCabalArguments = map makeArgument . unFlagAssignment
    where makeArgument :: (FlagName, Bool) -> String
          makeArgument (flag, isSet)
              | isSet = "-f" ++ (unFlagName flag)
              | otherwise = "-f-" ++ (unFlagName flag)
