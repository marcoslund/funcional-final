{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Category (viewCategory) where

import Models.Store
import Persistence.Initializer
import Persistence.Product
import Views.Template
import Views.Product

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query')
import Happstack.Server     ( ServerPart, Response, ok, lookRead, looks)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewCategory :: AcidState Store -> ServerPart Response
viewCategory acid =
    do categoryId   <- CategoryId <$> lookRead "categoryId"
       category <- query' acid (CategoryById categoryId)
       products <- query' acid (ProductsByCategoryId categoryId)
       mode <- looks "mode"
       case category of
            Nothing -> ok $ template "Category" [] (Mode "admin") $ do
                        H.div ! A.class_ "container" $ do
                            H.p $ do "No category found"
            (Just p@(Category{..}))  ->
                case mode of
                    ["admin"] ->
                        case products of
                                [] -> ok $ template "Category" [] (Mode "admin") $ do
                                            H.div ! A.class_ "container" $ do
                                                H.h1 ! A.class_ "title" $ H.toHtml categoryName
                                                H.div ! A.class_ "items-container" $ do
                                                    H.p $ do "No products found in this category"
                                _  -> ok $ template "Category" [] (Mode "admin") $ do
                                            H.div ! A.class_ "container" $ do
                                                H.h1 ! A.class_ "title" $ H.toHtml categoryName
                                                H.div ! A.class_ "items-container" $ do
                                                    mapM_ (viewProductContainer "admin") products
                    _ ->
                        case products of
                                [] -> ok $ template "Category" [] (Mode "user") $ do
                                            H.div ! A.class_ "container" $ do
                                                H.h1 ! A.class_ "title" $ H.toHtml categoryName
                                                H.div ! A.class_ "items-container" $ do
                                                    H.p $ do "No products found in this category"
                                _  -> ok $ template "Category" [] (Mode "user") $ do
                                            H.div ! A.class_ "container" $ do
                                                H.h1 ! A.class_ "title" $ H.toHtml categoryName
                                                H.div ! A.class_ "items-container" $ do
                                                    mapM_ (viewProductContainer "user") products
  where
    viewProductContainer mode Product{..} =
        let url = (H.toValue $ "/product?id=" ++ show (unProductId productId) ++ "&mode=" ++ mode)
        in  H.div ! A.class_ "product-container" $ do
                H.img ! A.src "images/product.png" ! A.alt "Product"
                H.div ! A.class_ "category-product-details" $ do
                    H.h2 $ H.a ! A.href url $ H.toHtml name
                    H.div ! A.class_ "category-product-price" $ do
                        H.p $ do "$"
                        H.p $ do H.toHtml price
