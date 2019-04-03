module Main where
    
import Control.Exception
import System.Exit

import Options.Applicative
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Core (parserUsage)

import System.Environment (getArgs)

import Prelude hiding (putStrLn)
import Data.Maybe (maybeToList)

import VabalMain
import VabalConfigure
import VabalUpdate
import VabalShow

import UserInterface

import qualified Paths_vabal as P(version)

import Data.Version (showVersion)

data Command = Update
             | Main [String] VabalMainArguments
             | Configure [String] [String]
             | Show VabalMainArguments

vabalVersion :: String
vabalVersion = showVersion P.version

vabalHeader :: String
vabalHeader = "vabal - The Cabal Companion, version " ++ vabalVersion

updateParserInfo :: ParserInfo Command
updateParserInfo = info (pure Update <**> helper)
                   ( fullDesc
                   <> progDesc updateProgDesc
                   <> header vabalHeader
                   )

configureParserInfo :: [String] -> [String] -> ParserInfo Command
configureParserInfo cabalArgs vabalArgs =
    info ((Configure cabalArgs vabalArgs <$ configureArgumentsParser) <**> helper)
    ( fullDesc
    <> header vabalHeader
    <> progDesc configureProgDesc
    )

showParserInfo :: ParserInfo Command
showParserInfo =
    info ((Show <$> showArgumentsParser) <**> helper)
    ( fullDesc
    <> progDesc showProgDesc
    <> header vabalHeader
    )

mainParserInfo :: [String] -> ParserInfo Command
mainParserInfo cmd =
    info ((Main cmd <$> mainArgumentsParser) <**> helper)
         ( fullDesc
         <> header vabalHeader
         <> progDesc mainProgDesc

         <> footerDoc (Just $ string "Available subcommands:"
                            <> linebreak
                            <> indent 2
                            ( string "vabal update (See vabal update --help)"
                              <> linebreak
                              <> string "vabal configure (See vabal configure --help)"
                              <> linebreak
                              <> string "vabal show (See vabal show --help)"
                            )
                      )
         )

updateExeName :: String -> ParserInfo a -> Maybe String -> ParserHelp -> ParserHelp
updateExeName name pinfo trailingArgs old =
    let p = infoParser pinfo
        desc = maybeToList . unChunk $ fmap (indent 2) (infoProgDesc pinfo)
        usage = case trailingArgs of
                  Just args ->
                    (parserUsage defaultPrefs p name <> string args) : desc
                  Nothing -> parserUsage defaultPrefs p name : desc
    in old { helpUsage = Chunk . Just $ vcat usage }

parseArgs :: [String] -> IO Command
parseArgs ("update" : args) =
    handleParseResult
    . overFailure (updateExeName "vabal update" updateParserInfo Nothing)
    $ execParserPure defaultPrefs updateParserInfo args

parseArgs ("configure" : args) =
    let (vabalArgs, otherArgs) = break (== "--") args
        cabalArgs = drop 1 otherArgs -- Ignore the '--'

        parserInfo = configureParserInfo cabalArgs vabalArgs

    in handleParseResult
       . overFailure (updateExeName "vabal configure" parserInfo (Just " [-- CABALARGS...]"))
       $ execParserPure defaultPrefs parserInfo vabalArgs

parseArgs ("show" : args) =
    handleParseResult
    . overFailure (updateExeName "vabal show" showParserInfo Nothing)
    $ execParserPure defaultPrefs showParserInfo args

parseArgs args =
    let (vabalArgs, otherArgs) = break (== "--") args
        cmd = drop 1 otherArgs -- Ignore the '--'
        parserInfo = mainParserInfo cmd

    in handleParseResult
    . overFailure (updateExeName "vabal" parserInfo (Just " [-- COMMAND ARGS...]"))
    $ execParserPure defaultPrefs parserInfo vabalArgs


main :: IO ()
main = do
    cmd <- getArgs >>= parseArgs

    let errorHandler :: SomeException -> IO ()
        errorHandler ex = do
            writeError $ show ex
            exitWith (ExitFailure 1)

    handle errorHandler $
        case cmd of
            Update -> vabalUpdate
            Main c args -> vabalMain c args
            Configure cabalArgs args -> vabalConfigure cabalArgs args
            Show args -> vabalShow args

