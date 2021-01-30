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
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

home :: AcidState Store -> ServerPart Response
home acid =
    do published <- query' acid (ProductsByStatus Published) -- TODO delete
       ok $ template "home" [] $ do
            H.div ! A.class_ "home-img" $ do
                H.div ! A.class_ "home-img-container" $ do
                    H.h1 ! A.class_ "home-title" $ "Our best products just one click away from you"
                    