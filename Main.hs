{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Main where

import Routes               (route)
import Persistence.Product  (initialStoreState)

import Control.Exception    (bracket)
import Data.Acid            (openLocalState)
import Data.Acid.Local      (createCheckpointAndClose)
import Data.IxSet           (Indexable(..))
import Happstack.Server     ( nullConf, simpleHTTP)

import           Logging.Config.Json (getManager)
import           Logging.Global      (run)

main :: IO ()
main = getManager "{}" >>= flip run app

logger = "Main"

app :: IO ()
app =
    do bracket (openLocalState initialStoreState)
               (createCheckpointAndClose)
               (\acid ->
                    simpleHTTP nullConf (route acid))
