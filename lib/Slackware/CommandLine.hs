-- | Command line option parsing.
module Slackware.CommandLine
    ( Program(..)
    , execOptsParser
    , execOptsParserPure
    ) where

import Control.Applicative (optional)
import Options.Applicative
    ( Parser
    , ParserInfo
    , ParserResult
    , (<**>)
    , command
    , defaultPrefs
    , execParser
    , execParserPure
    , header
    , helper
    , info
    , metavar
    , progDesc
    , strArgument
    , subparser
    )

-- | Represents all possible command line options.
data Program
    = Build -- ^ Build all packages.
    | DownloadSource -- ^ Download all sources.
    | Install -- ^ Install prebuilt packages.
    -- | Upgrade all packages. The argument is an optional Gnome version.
    | UpgradeAll (Maybe String)
    deriving Eq

instance Show Program where
    show Build = "build"
    show DownloadSource = "download"
    show Install = "install"
    show (UpgradeAll gnomeVersion) =
        "update-gnome" ++ maybe "" (' ' :) gnomeVersion

program :: Parser Program
program = subparser
       ( command "build"
         (info (pure Build)
               (progDesc "Build all packages"))
       <> command "download-source"
         (info (pure DownloadSource)
               (progDesc "Download all sources"))
       <> command "install"
         (info (pure Install)
               (progDesc "Install built packages"))
       <> command "update-gnome"
         (info (UpgradeAll <$> optional (strArgument (metavar "VERSION")))
               (progDesc "Upgrade a package"))
       )

opts :: ParserInfo Program
opts = info (program <**> helper) (header "Dlackware Build System")

-- | Parse actual command line options.
execOptsParser :: IO Program
execOptsParser = execParser opts

-- | Parse command line options given in the first argument.
execOptsParserPure :: [String] -> ParserResult Program
execOptsParserPure = execParserPure defaultPrefs opts
