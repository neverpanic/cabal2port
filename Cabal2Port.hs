module Cabal2Port (run) where

-- import Control.Monad.Exception
import Distribution.Compiler
import Distribution.InstalledPackageInfo
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.ParseUtils (PWarning)
import Distribution.System
import Distribution.Version
import Network.HTTP
    ( simpleHTTP
    , getRequest
    , getResponseBody )
import System.Exit
import System.IO
import System.Info
import Text.Printf

import Cabal2Port.Options
import Cabal2Port.Output
import Cabal2Port.Error
import Cabal2Port.Portfile

hackageURL = "http://hackage.haskell.org/package/%s/%s.cabal"

{--
Given a cabal package name, generate a Portfile and write it to the appropriate
location on disk relative to the porttree parameter in the Options structure.
Overwrites existing packages.

@param opts
    options given to the cabal2port executable
@param port
    name of the haskell package to port or update
@return
    a monaderror over a list of strings; the list of strings is a list of
    warnings generated while parsing the cabal package description
--}
--run :: (MonadError C2PError m) => Options -> String -> m [String]
run :: Options -> String -> IO ()
run opts port = do
    -- fetch the package description file
    fetchCabalDescription port
        >>= parseCabalDescription
        >>= finalizeCabalDescription
        >>= convertCabalDescriptionToPortfile
        >>= updatePortfile

    -- deal with errors
    case cabalFile of
        Left error          -> warning opts $ printf "Failed to fetch package description for %s: %s" port error
        Right cabalResponse -> do
            -- success, parse the package description
            case parsePackageDescription (getResponseBody cabalResponse) of
                -- deal with parse errors
                ParseFailed error ->
                    warning opts $ printf "Failed to parse package description for %s: %s" port (show error)
                -- success parsing (there might be warnings, though)
                ParseOk warnings genericPackageDescription -> do
                    -- deal with the warnings, but ignore the return value
                    mapM_ printWarning warnings
                    -- convert to portfile and write
                    return genericPackageDescription
                        >>= finalize opts port
                        >>= convertToPortfile opts port
                        >>= updatePortfile opts port
            where
                -- helper function to print parse warnings
                printWarning :: PWarning -> IO ()
                printWarning pwarn =
                    warning opts $ printf "While parsing package description for %s: %s" port $ show pwarn
    where
        cabalFileURL = printf hackageURL port port

{-
 - Download a cabal description given a the name of a haskell package.
 -
 - @param name
 -        Name of the haskell package whose cabal description should be fetched.
 - @returns
 -        A string containing the cabal description file. Raises an exception of class
 -        C2PError.FetchFailure if the download failed (e.g. because the package name was invalid
 -        and no package with the given name exists). More information about the error may be found
 -        in the string argument to the FetchFailure constructor.
 -}
fetchCabalDescription :: String -> IO String
fetchCabalDescription port =
    case simpleHTTP (getRequest (getCabalDescriptionURL port)) of
        Left error     -> throwIO FetchFailure $ error
        Right response -> return (getResponseBody response)

{-
 - Return an URL pointing to the cabal file of a haskell package on hackage.haskell.org
 -
 - @param name
 -        Name of the haskell package to be downloaded
 - @returns
 -        An URL string that should be pointing to the cabal description file, if the given package
 -        name is valid
 -}
getCabalDescriptionURL :: String -> String
getCabalDescriptionURL port = printf hackageURL port port

-- finalize the generic package description into a non-generic package description
finalize :: Options -> String -> GenericPackageDescription -> Either String PackageDescription
finalize opts port gpd = do
    let flags    = [(FlagName "splitBase", True)]
    let truefunc = const True
    let platform = Platform buildArch buildOS
    let compiler = CompilerId buildCompilerFlavor compilerVersion

    case finalizePackageDescription flags truefunc platform compiler [] gpd of
        Left missingdeps  -> do
            fail $ printf "Missing dependencies while finalizing package description for %s: %s" port (show missingdeps)
        Right (pd, flags) -> do
            return pd

-- given a generic package description, convert to a portfile representation
convertToPortfile :: Options -> String -> Maybe PackageDescription -> IO (Maybe Portfile)
convertToPortfile opts port Nothing = return Nothing
convertToPortfile opts port (Just pd) = return (Just (buildPortfile pd))

buildPortfile :: PackageDescription -> Portfile
buildPortfile pd =
    defaultPortfile

updatePortfile :: Options -> String -> Maybe Portfile -> IO ()
updatePortfile opts port Nothing =
    critical opts $ printf "failed to generate Portfile for %s" port
updatePortfile opts port (Just portfile) = putStrLn $ show portfile
