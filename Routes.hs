{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Routes (route) where

import Models.Blog
import Views.Home
import Views.Edit

import Control.Monad        (msum)
import Data.Acid            (AcidState)
import Happstack.Server     ( ServerPart, Response, decodeBody, defaultBodyPolicy, dir, notFound, nullDir, toResponse)

-- | route incoming requests
route :: AcidState Blog -> ServerPart Response
route acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , dir "edit"        $ edit acid
            , dir "new"         $ new acid
            , dir "view"        $ view acid
            , dir "drafts"      $ drafts acid
            , nullDir          >> home acid
            ]