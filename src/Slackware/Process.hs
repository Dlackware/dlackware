module Slackware.Process
    ( outErrProcess
    , runSlackBuild
    ) where

import Conduit (ConduitM, (.|), runConduit, sourceHandle)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 as C8
import Data.Void (Void)
import GHC.IO.Handle (Handle, hClose)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process
    ( CreateProcess(..)
    , CmdSpec(..)
    , StdStream(..)
    , createPipe
    , createProcess
    , waitForProcess
    )
import System.Exit (ExitCode(..))

outErrProcess :: (CreateProcess, Handle) -> ConduitM C8.ByteString Void IO a -> IO ExitCode
outErrProcess (process, source) consumer = do
    (_, _, _, processHandle) <- liftIO $ createProcess process

    _ <- runConduit $ sourceHandle source .| consumer
    _ <- hClose source
    waitForProcess processHandle

runSlackBuild :: FilePath -> [(String, String)] -> IO (CreateProcess, Handle)
runSlackBuild slackBuild environment = do
    old <- getEnvironment
    (readEnd, writeEnd) <- createPipe

    return (CreateProcess
        { cmdspec = RawCommand ("." </> slackBuild) []
        , cwd = Nothing
        , env = Just $ mappend old environment
        , std_in = Inherit
        , std_out = UseHandle writeEnd
        , std_err = UseHandle writeEnd
        , close_fds = False
        , create_group = False
        , delegate_ctlc = False
        , detach_console = False
        , create_new_console = False
        , new_session = False
        , child_group = Nothing
        , child_user = Nothing
        , use_process_jobs = False
        }, readEnd)
