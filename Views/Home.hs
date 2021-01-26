{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Home (home) where

import Models.Store
import Persistence.Product
import Views.Template
import Views.Product

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query')
import Happstack.Server     ( ServerPart, Response, ok)

-- | render all the Published products (ordered newest to oldest)
home :: AcidState Store -> ServerPart Response
home acid =
    do published <- query' acid (ProductsByStatus Published)
       ok $ template "home" [] $ do
         mapM_ productHtml published
