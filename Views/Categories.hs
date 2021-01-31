{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Categories (viewCategories) where

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

viewCategories :: AcidState Store -> ServerPart Response
viewCategories acid =
    do categories <- query' acid (ProductsByStatus Published) -- TODO CHANGE TO GET CATEGORIES
       ok $ template "Categories" [] $ do
            H.div ! A.class_ "container" $ do
                H.h1 ! A.class_ "title" $ "Categories"
                H.div ! A.class_ "items-container" $ do
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Food Market"
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Electronics"
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Clothing"
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Furniture"
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Vehicles"
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Hardware & Tools"
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Books"
                    H.div ! A.class_ "category-container" $ H.a ! A.href "/category" $ do
                        H.h2 ! A.class_ "category-title" $ "Beauty & Makeup"
                    