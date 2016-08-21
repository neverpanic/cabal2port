module Cabal2Port.Output
    ( critical
    , warning
    , progress
    , verbose
    , debug
    ) where

import System.IO
    ( hPutStrLn
    , stderr )
import Control.Monad
    ( when )

import Cabal2Port.Options

critical, warning, progress, verbose, debug :: Options -> String -> IO ()
printWithPrefix :: String -> String -> IO ()

printWithPrefix prefix message =
    hPutStrLn stderr $ prefix ++ message

-- keep all of the prefixes aligned
critical opts =                                 printWithPrefix "  Error: "
warning  opts =                                 printWithPrefix "Warning: "
progress opts =                                 printWithPrefix ""
verbose  opts message = when (optVerbose opts) (printWithPrefix "Verbose: " message)
debug    opts message = when (optDebug   opts) (printWithPrefix "  Debug: "   message)
