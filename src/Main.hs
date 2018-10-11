module Main where

import           Arch (grepSlackBuild)
import           CompileOrder ( Step(..)
                              , parseCompileOrder
                              )
import           Config ( Config(..)
                        , parseConfig
                        )
import           Data.Either (fromRight)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import           Package (parseInfoFile)
import System.Directory ( createDirectoryIfMissing
                        , withCurrentDirectory
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )
import System.Environment (getEnvironment)
import System.FilePath ( FilePath
                       , (</>)
                       , (<.>)
                       , joinPath
                       , takeDirectory
                       )
import System.Process ( CreateProcess(..)
                      , StdStream(..)
                      , CmdSpec(..)
                      , createProcess
                      , waitForProcess
                      , callProcess
                      , readProcess
                      , readProcessWithExitCode
                      )
import Text.ParserCombinators.Parsec (parse)

runSlackBuild :: FilePath -> [(String, String)] -> IO CreateProcess
runSlackBuild slackBuild env = do
    oldEnv <- getEnvironment

    return $ CreateProcess
        { cmdspec = RawCommand ("." </> slackBuild) []
        , cwd = Nothing
        , env = Just $ mappend oldEnv env
        , std_in = Inherit
        , std_out = Inherit
        , std_err = Inherit
        , close_fds = False
        , create_group = False
        , delegate_ctlc = False
        , detach_console = False
        , create_new_console = False
        , new_session = False
        , child_group = Nothing
        , child_user = Nothing
        , use_process_jobs = False
        }

buildPackage :: FilePath -> (String, String) -> IO ()
buildPackage repo (old, pkg) = do
    let infoFile = joinPath [repo, pkg, pkg <.> "info"]
    content <- readFile infoFile

    let tuple = case parse parseInfoFile infoFile content of
                  Left left -> error $ show left
                  Right right -> right

    let slackBuild = pkg <.> "SlackBuild"

    let (_, version, _, downloads, _) = tuple

    withCurrentDirectory (repo </> pkg) (callProcess "wget" ("-nc" : downloads))

    oldDirectory <- getCurrentDirectory
    setCurrentDirectory $ repo </> pkg

    (buildNumber, archNoarch) <- grepSlackBuild <$> (readFile slackBuild)
    let arch = if (length archNoarch) > 1 then archNoarch else "x86_64"

    let fullPath = "/var/cache/dlackware/" ++ pkg ++ "-" ++ version ++ "-"
                ++ arch ++ "-" ++ buildNumber ++ "_dlack.txz"

    (_, _, _, processHandle) <- (runSlackBuild slackBuild [("VERSION", version)]) >>= createProcess
    _ <- waitForProcess processHandle

    setCurrentDirectory oldDirectory

    callProcess "/sbin/upgradepkg" ["--reinstall", "--install-new", old ++ fullPath]

buildCompileOrder :: String -> IO ()
buildCompileOrder compileOrder = do
    content <- readFile compileOrder

    mapM_ (buildPackage $ takeDirectory compileOrder) (pkgs content)
        where pkgs content = foldr f [] $ fromRight [] $ parseCompileOrder compileOrder content
              f (PackageName Nothing new) acc = ("", new) : acc
              f (PackageName (Just old) new) acc = (old ++ "%", new) : acc

build :: IO ()
build = do
    createDirectoryIfMissing False "/tmp/dlackware"

    configContent <- BS.readFile "dlackware.yaml"
    let config = fromRight undefined $ parseConfig configContent
    let compileOrders = fmap f (repos config)
            where f x = (T.unpack $ reposRoot config) </> (T.unpack x)

    mapM_ buildCompileOrder compileOrders

main :: IO ()
main = build
