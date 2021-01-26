{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Routes (route) where

import Models.Store
import Views.Home
import Views.Edit

import Control.Monad        (msum)
import Data.Acid            (AcidState)
import Happstack.Server     ( ServerPart, Response, decodeBody, defaultBodyPolicy, dir, notFound, nullDir, toResponse, serveDirectory, Browsing( DisableBrowsing ))

-- | route incoming requests
route :: AcidState Store -> ServerPart Response
route acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , dir "edit"        $ edit acid
            , dir "new"         $ new acid
            , dir "view"        $ view acid
            , dir "drafts"      $ drafts acid
            , dir "images"      $ serveDirectory DisableBrowsing ["index.html"] "images"
            , nullDir          >> home acid
            ]