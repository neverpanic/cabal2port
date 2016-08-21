module Main (main) where

import System.Console.GetOpt
    ( usageInfo )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitWith
    , ExitCode(ExitFailure, ExitSuccess) )
import System.IO
    ( hSetBuffering
    , hPutStr
    , stdout
    , stderr
    , BufferMode(LineBuffering) )

import Control.Monad
    ( when )

import qualified Data.Maybe

import Cabal2Port.Options
    ( Options
    , parseOpts
    , optShowUsage
    , optShowVersion
    , usageHeader
    , options )
import Cabal2Port.Version
    ( version )
import Cabal2Port
    ( run )

main :: IO ()
main = do
    -- enable line buffering; cabal does that for speed, might be a good idea for us, too
    hSetBuffering stdout LineBuffering
    -- read arguments, parse them and hand them to the main worker
    getArgs >>= parseOpts >>= mainWorker

-- handle some standard options like --help and --version, ensure the list of
-- package names isn't empty, then pass on to port
mainWorker :: (Options, [String]) -> IO ()
mainWorker (opts, args) = do
    -- handle --help/-h
    when (optShowUsage opts) $ printUsage
    -- handle --version/-V
    when (optShowVersion opts) $ do
        hPutStr stderr $ "cabal2port " ++ version
        exitWith ExitSuccess
    case args of
        -- handle an empty package list
        [] -> printUsage
        -- default case: pass to port
        _  -> port opts args
    where printUsage = do
        hPutStr stderr $ usageInfo usageHeader options
        exitWith (ExitFailure 1)

-- generate Portfiles for a list of package names
port :: Options -> [String] -> IO ()

-- given a non-empty list, call cabal2port for each package name and recurse
port opts (x:xs) = do
    run opts x
    port opts xs

-- an empty list is not an error (because the error condition is already
-- handled in mainWorker), but only means we've succeeded in processing
port opts [] = do
    exitWith ExitSuccess
