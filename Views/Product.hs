{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Product (viewProduct) where

import Models.Store
import Persistence.Initializer
import Persistence.Product
import Views.Template

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query')
import Happstack.Server     ( ServerPart, Method(POST, GET), Response, lookRead, lookText, method, notFound, ok, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewProduct  :: AcidState Store -> ServerPart Response
viewProduct acid = do
   pid   <- ProductId <$> lookRead "id"
   mProd <- query' acid (ProductById pid)
   case mProd of
      Nothing ->
         notFound $ template "Not found" [] (Mode "admin") $
                do "Could not find a product with ID "
                   H.toHtml (unProductId pid)
      (Just p@(Product{..})) -> 
         do method GET
            mode   <- Mode <$> lookRead "mode"
            case mode of
               Mode "admin" ->
                  ok $ template "Product" [] (Mode "admin") $ do
                     H.button ! A.class_ "back-btn" $ H.a ! A.href (H.toValue $ "/products?categoryId=" ++ show (unCategoryId category) ++ "&mode=admin") $ "Go back"
                     H.div ! A.class_ "profile-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product" ! A.class_ "product-image"
                        H.div ! A.class_ "product-details" $ do
                              H.h2 ! A.class_ "product-name" $ H.toHtml name
                              H.div ! A.class_ "detail-container" $ do
                                 H.p ! A.class_ "detail-title" $ do "Price:"
                                 H.p ! A.class_ "detail-price" $ do "$"
                                 H.p ! A.class_ "detail-price" $ do H.toHtml price
                              H.div ! A.class_ "detail-container" $ do
                                 H.p ! A.class_ "detail-title" $ do "In stock:"
                                 H.p $ do H.toHtml stock
                                 H.p ! A.class_ "white-space-pre" $ do " units"
                              H.div ! A.class_ "detail-container" $ do
                                 H.p ! A.class_ "detail-title" $ do "Brand:"
                                 H.p $ do H.toHtml brand
                              H.div ! A.class_ "detail-container-column" $ do
                                 H.p ! A.class_ "detail-title" $ do "About this item"
                                 H.p ! A.class_ "detail-indent" $ do H.toHtml description
                              H.div ! A.class_ "add-cart-btn-container" $ do
                                 H.button ! A.name "edit" ! A.value "edit" ! A.class_ "action-button" $ H.a ! A.href (H.toValue $ "/edit?id=" ++ show (unProductId pid) ++ "&mode=admin") $ "Edit"
               Mode "user" ->
                  ok $ template "Product" [] (Mode "user") $ do
                     H.button ! A.class_ "back-btn" $ H.a ! A.href (H.toValue $ "/products?categoryId=" ++ show (unCategoryId category) ++ "&mode=user") $ "Go back"
                     H.div ! A.class_ "profile-container" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product" ! A.class_ "product-image"
                        H.div ! A.class_ "product-details" $ do
                              H.h2 ! A.class_ "product-name" $ H.toHtml name
                              H.div ! A.class_ "detail-container" $ do
                                 H.p ! A.class_ "detail-title" $ do "Price:"
                                 H.p ! A.class_ "detail-price" $ do "$"
                                 H.p ! A.class_ "detail-price" $ do H.toHtml price
                              H.div ! A.class_ "detail-container" $ do
                                 H.p ! A.class_ "detail-title" $ do "In stock:"
                                 H.p $ do H.toHtml stock
                                 H.p ! A.class_ "white-space-pre" $ do " units"
                              H.div ! A.class_ "detail-container" $ do
                                 H.p ! A.class_ "detail-title" $ do "Brand:"
                                 H.p $ do H.toHtml brand
                              H.div ! A.class_ "detail-container-column" $ do
                                 H.p ! A.class_ "detail-title" $ do "About this item"
                                 H.p ! A.class_ "detail-indent" $ do H.toHtml description
                              H.div ! A.class_ "add-cart-btn-container" $ do
                                 H.form ! A.enctype "multipart/form-data"
                                    ! A.method "POST"
                                    ! A.action (H.toValue $ "/addtocart?id=" ++ show (unProductId pid)) $ H.button ! A.name "addtocart" ! A.value "addtocart" ! A.class_ "action-button" $ "Add to cart"