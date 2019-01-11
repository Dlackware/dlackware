{-# LANGUAGE OverloadedStrings #-}
module Slackware.Log ( Level(..)
                     , console
                     ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types ( Color(..)
                                 , ColorIntensity(..)
                                 , ConsoleIntensity(..)
                                 , ConsoleLayer(..)
                                 , SGR(..)
                                 )

data Level = Info
           | Warn
           | Fatal

console :: Level -> T.Text -> IO ()
console level msg = do
    let colorful = setSGR [ SetConsoleIntensity BoldIntensity
                          , SetColor Foreground Vivid $ getColor level
                          ]
    T.IO.putStrLn $ T.concat [colorful, msg, setSGR []]
        where getColor Info = Green
              getColor Warn = Yellow
              getColor Fatal = Red
              setSGR = T.pack . setSGRCode
