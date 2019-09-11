module Main where

import Control.Applicative (Alternative(..))
import Options.Applicative ( Parser
                           , ParserInfo
                           , execParser
                           , subparser
                           , strArgument
                           , helper
                           , (<**>)
                           , command
                           , info
                           , progDesc
                           , header
                           , metavar
                           )
import Slackware.Command
import Slackware.Upgrade

data Program = Build
             | DownloadSource
             | Install
             | Upgrade String String
             | UpgradeAll

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
       <> command "upgrade"
         (info (upgradeParser <|> pure UpgradeAll)
               (progDesc "Upgrade a package (EXPERIMENTAL)"))
       )
  where
    upgradeParser = Upgrade
        <$> strArgument (metavar "NAME")
        <*> strArgument (metavar "VERSION")

run :: Program -> IO ()
run Build = build
run DownloadSource = downloadSource
run Install = install
run (Upgrade pkgnam version) = upgrade pkgnam version
run UpgradeAll = upgradeAll

opts :: ParserInfo Program
opts = info (program <**> helper) (header "Dlackware Build System")

main :: IO ()
main = execParser opts >>= run
