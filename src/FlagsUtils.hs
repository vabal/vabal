module FlagsUtils where

import Distribution.Types.GenericPackageDescription
import VabalError

makeFlagName :: String -> FlagName
makeFlagName "" = throwVabalError "Empty string is not a valid flag."
makeFlagName flag = makeFlagName flag

parseFlag :: String -> (FlagName, Bool)
parseFlag ('+':flag) = (makeFlagName flag, True)
parseFlag ('-':flag) = (makeFlagName flag, False)
parseFlag flag       = (makeFlagName flag, True)

makeFlagAssignment :: [String] -> FlagAssignment
makeFlagAssignment = mkFlagAssignment . map parseFlag


makeCabalArguments :: FlagAssignment -> [String]
makeCabalArguments = map makeArgument . unFlagAssignment
    where makeArgument :: (FlagName, Bool) -> String
          makeArgument (flag, isSet)
              | isSet = "-f" ++ (unFlagName flag)
              | otherwise = "-f-" ++ (unFlagName flag)
