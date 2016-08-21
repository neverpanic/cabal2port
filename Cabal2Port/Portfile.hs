module Cabal2Port.Portfile
    ( Portfile(..)
    , PortfileLicense(..)
    , PortfileChecksum(..)
    , PortfileDependency(..)
    , defaultPortfile
    ) where

import Data.Version
    ( Version
    , makeVersion )
import Data.Maybe

data PortfileLicense
    = GPL (Maybe Version)
    | AGPL (Maybe Version)
    | LGPL (Maybe Version)
    | BSD
    | MIT
    | Apache (Maybe Version)
    | PublicDomain
    | UnknownLicense String
    deriving (Show, Eq)

data PortfileChecksum
    = SHA256 String
    | RMD160 String
    deriving (Show, Eq)

data PortfileDependency
    = DepPort String
    | DepPath FilePath
    | DepLib  FilePath
    | DepBin  FilePath
    deriving (Show, Eq)

data Portfile = Portfile
    { portsystem       :: Version
    , portgroups       :: [(String, Version)]
    , name             :: String
    , version          :: Version
    , maintainers      :: [String]
    , checksums        :: [PortfileChecksum]
    , platforms        :: [String]
    , license          :: PortfileLicense
    , description      :: String
    , long_description :: String
    , depends_lib      :: [PortfileDependency]
    , depends_build    :: [PortfileDependency]
    } deriving (Show, Eq)

defaultPortfile = Portfile
    { portsystem       = makeVersion [1, 0]
    , portgroups       = [("haskell", makeVersion [1, 0])]
    , name             = "unnamed"
    , version          = makeVersion [0]
    , maintainers      = ["cal", "openmaintainer"]
    , checksums        = []
    , platforms        = ["darwin"]
    , license          = UnknownLicense "fixme"
    , description      = ""
    , long_description = "${description}"
    , depends_lib      = []
    , depends_build    = []
    }
