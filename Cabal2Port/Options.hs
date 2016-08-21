module Cabal2Port.Options
    ( Options(..)
    , defaultOptions
    , options
    , usageHeader
    , parseOpts
    ) where

import System.Console.GetOpt
    ( OptDescr(Option)
    , ArgDescr(NoArg, ReqArg)
    , ArgOrder(RequireOrder)
    , getOpt
    , usageInfo )
import System.IO
    ( hPutStr
    , stderr )
import System.Exit
    ( exitWith
    , ExitCode(ExitFailure) )

-- possible arguments
data Options = Options
    { optVerbose     :: Bool
    , optDebug       :: Bool
    , optShowVersion :: Bool
    , optShowUsage   :: Bool
    , optPortTree    :: Maybe FilePath
    } deriving Show

-- default values for the options
defaultOptions = Options
    { optVerbose     = False
    , optDebug       = False
    , optShowVersion = False
    , optShowUsage   = False
    , optPortTree    = Nothing
    }

-- option specifications, associates option letters and strings with functions
-- that transform an options structure
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v'] ["verbose"]
        (NoArg (\ opts -> opts { optVerbose = True }))
        "enable verbose mode"
    , Option ['d'] ["debug"]
        (NoArg (\ opts -> opts { optVerbose = True, optDebug = True}))
        "enable debug mode"
    , Option ['V'] ["version"]
        (NoArg (\ opts -> opts { optShowVersion = True}))
        "print version number"
    , Option ['h'] ["help"]
        (NoArg (\ opts -> opts { optShowUsage = True}))
        "show usage information"
    , Option ['p'] ["porttree"]
        (ReqArg (\ path opts -> opts { optPortTree = Just path }) "DIR")
        "use port tree in DIR"
    ]

-- header to be printed above getopt usage messages
usageHeader = "Usage: cabal2port [OPTION...] package..."

-- parse the options from a list of strings into a tuple of (Options, [String])
-- where the list of strings is the list of positional arguments
parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
    case getOpt RequireOrder options argv of
        -- applies the functions specified in options to the default options
        -- structure
        (o,n,[])   -> return (foldl (flip id) defaultOptions o, n)
        -- prints an error message and aborts
        (_,_,errs) -> do
            hPutStr stderr $ concat errs ++ usageInfo usageHeader options
            exitWith (ExitFailure 1)

