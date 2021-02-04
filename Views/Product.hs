{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Product (viewProduct) where

import Models.Store
import Persistence.Product
import Views.Template

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query')
import Happstack.Server     ( ServerPart, Response, ok)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewProduct  :: AcidState Store -> ServerPart Response
viewProduct acid =
   ok $ template "Product" [] $ do
      H.button ! A.class_ "back-btn" $ H.a ! A.href "/category" $ "Go back"
      H.div ! A.class_ "profile-container" $ do
         H.img ! A.src "images/product.png" ! A.alt "Product" ! A.class_ "product-image"
         H.div ! A.class_ "product-details" $ do
               H.h2 ! A.class_ "product-name" $ "Product X"
               H.div ! A.class_ "detail-container" $ do
                  H.p ! A.class_ "detail-title" $ do "Price:"
                  H.p ! A.class_ "detail-price" $ do "$ 2.899"
               H.div ! A.class_ "detail-container" $ do
                  H.p ! A.class_ "detail-title" $ do "In stock:"
                  H.p $ do "3"
                  H.p ! A.class_ "white-space-pre" $ do " units"
               H.div ! A.class_ "detail-container" $ do
                  H.p ! A.class_ "detail-title" $ do "Brand:"
                  H.p $ do "Amazon Basics"
               H.div ! A.class_ "detail-container-column" $ do
                  H.p ! A.class_ "detail-title" $ do "About this item"
                  H.p ! A.class_ "detail-indent" $ do "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
               H.div ! A.class_ "add-cart-btn-container" $ do
                  H.button ! A.name "addToCart" ! A.value "addToCart" ! A.class_ "action-button" $ "Add to cart"