module Main where

import Control.Exception (try)
import Slackware.Command
import Slackware.CommandLine
import Slackware.Error
import Slackware.Log

runCommand :: Program -> IO ()
runCommand Build = build
runCommand DownloadSource = downloadSource
runCommand Install = install
runCommand (UpgradeAll gnomeVersion) = updateGnome gnomeVersion

run :: Program -> IO ()
run command = tryCommand
    >>= either (console Fatal . showPackageError) pure
  where
    tryCommand :: IO (Either BuildSystemException ())
    tryCommand = try $ runCommand command

main :: IO ()
main = execOptsParser >>= run
