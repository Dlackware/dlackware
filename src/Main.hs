module Main where

import Slackware.Command ( build
                         , downloadSource
                         , install
                         )
import Options.Applicative ( Parser
                           , ParserInfo
                           , execParser
                           , subparser
                           , helper
                           , (<**>)
                           , command
                           , info
                           , progDesc
                           , header
                           )

data Program = Build
             | DownloadSource
             | Install

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
       )

run :: Program -> IO ()
run Build = build
run DownloadSource = downloadSource
run Install = install

opts :: ParserInfo Program
opts = info (program <**> helper) (header "Dlackware Build System")

main :: IO ()
main = execParser opts >>= run
