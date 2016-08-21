module Cabal2Port.Error
    ( C2PError(..) ) where

import Control.Monad.Error

data C2PError
    = FetchFailure String
    | ParseFailure String
    | MissingDependencies String
    | MiscError String

instance Show C2PError where
    show (FetchFailure str)        = "Failed to fetch package description: " ++ str
    show (ParseFailure str)        = "Failed to parse package description: " ++ str
    show (MissingDependencies str) = "Missing dependencies while finalizing package description: " ++ str
    show (MiscError str)           = str

instance Error C2PError where
    noMsg      = MiscError "Unknown error"
    strMsg str = MiscError str
