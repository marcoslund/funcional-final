{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Category (viewCategory) where

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

viewCategory :: AcidState Store -> ServerPart Response
viewCategory acid =
    do products <- query' acid (ProductsByStatus Published) -- TODO CHANGE TO GET CATEGORIES
       ok $ template "Category" [] $ do
            H.div ! A.class_ "container" $ do
                H.h1 ! A.class_ "title" $ "Category"
                H.div ! A.class_ "items-container" $ do
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    H.div ! A.class_ "product-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.div ! A.class_ "category-product-details" $ do
                            H.h2 $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "$ 2.899"
                    