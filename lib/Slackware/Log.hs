module Slackware.Log
    ( Level(..)
    , console
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types
    ( Color(..)
    , ColorIntensity(..)
    , ConsoleIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    )
import System.IO (stderr)

data Level
    = Info
    | Warn
    | Fatal

color :: Level -> T.Text -> T.Text
color level msg = T.concat
    [ setSGR [ SetConsoleIntensity BoldIntensity
                , SetColor Foreground Vivid $ getColor level
                ]
    , msg
    , setSGR []
    ]
  where
    setSGR = T.pack . setSGRCode
    getColor Info = Green
    getColor Warn = Yellow
    getColor Fatal = Red

console :: Level -> T.Text -> IO ()
console Info msg = T.IO.putStrLn $ color Info msg
console level msg = T.IO.hPutStrLn stderr $ color level msg
