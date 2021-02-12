{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Checkout (viewCheckout, addToCart) where

import Models.Store
import Persistence.Initializer
import Persistence.Product
import Views.Template

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query', update')
import Happstack.Server     ( ServerPart, Method(GET, POST), Response, ok, lookRead, method, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewCheckout  :: AcidState Store -> ServerPart Response
viewCheckout acid =
   ok $ template "Checkout" [] (Mode "admin") $ do
       H.div ! A.class_ "container" $ do
            H.h1 ! A.class_ "title" $ "Checkout"
            H.div ! A.class_ "checkout-container" $ do
                H.div ! A.class_ "shopping-cart-container" $ do
                    H.h2 $ "Shopping Cart"
                    H.div ! A.class_ "shopping-cart-headers" $ do
                        H.p ! A.class_ "shopping-cart-item-label" $ do "Product details"
                        H.p ! A.class_ "shopping-cart-item-qty" $ do "Quantity"
                        H.p ! A.class_ "shopping-cart-item-price" $ do "Total"
                    H.div ! A.class_ "shopping-cart-items" $ do
                        H.div ! A.class_ "shopping-cart-item" $ do
                            H.div ! A.class_ "shopping-cart-item-label" $ do
                                H.img ! A.src "images/product.png" ! A.alt "Product"
                                H.p $ H.a ! A.href "/product" $ "Roku Express | HD Streaming Media Player with High Speed HDMI Cable and Simple Remote"
                            H.p ! A.class_ "shopping-cart-item-qty" $ do "2"
                            H.p ! A.class_ "shopping-cart-item-price" $ do "$2.899"
                        H.div ! A.class_ "shopping-cart-item" $ do
                            H.div ! A.class_ "shopping-cart-item-label" $ do
                                H.img ! A.src "images/product.png" ! A.alt "Product"
                                H.p $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "2"
                            H.p $ do "$2.899"
                        H.div ! A.class_ "shopping-cart-item" $ do
                            H.div ! A.class_ "shopping-cart-item-label" $ do
                                H.img ! A.src "images/product.png" ! A.alt "Product"
                                H.p $ H.a ! A.href "/product" $ "Product"
                            H.p $ do "2"
                            H.p $ do "$2.899"
                H.div ! A.class_ "payment-container" $ do
                    H.h2 $ "Order Summary"
                    H.div ! A.class_ "payment-subtotal" $ do
                        H.p $ do "Subtotal"
                        H.p $ do "$2.899"
                    H.div ! A.class_ "payment-shipping" $ do
                        H.p $ do "Shipping"
                        H.p $ do "$30"
                    H.div ! A.class_ "payment-total" $ do
                        H.p $ do "Estimated Total"
                        H.p $ do "$2.929"
                    H.div ! A.class_ "purchase-btn-container" $ do
                        H.button ! A.name "purchaseOrder" ! A.value "purchaseOrder" ! A.class_ "confirm-button" $ "Purchase"

addToCart :: AcidState Store -> ServerPart Response
addToCart acid = do
  method POST
  pid   <- ProductId <$> lookRead "id"
  
  update' acid (AppendProduct pid 1)
  let url = "/product?id=" ++ show (unProductId $ pid)
  seeOther url (toResponse ())