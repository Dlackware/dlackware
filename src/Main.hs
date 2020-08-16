module Main where

import Slackware.Command
import Slackware.CommandLine
import Slackware.Upgrade

run :: Program -> IO ()
run Build = build
run DownloadSource = downloadSource
run Install = install
run (Upgrade pkgnam version) = upgrade pkgnam version Nothing
run (UpgradeAll gnomeVersion) = upgradeAll gnomeVersion

main :: IO ()
main = execOptsParser >>= run
