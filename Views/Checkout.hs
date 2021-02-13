{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Checkout (viewCheckout, addToCart, purchase) where

import Models.Store
import Persistence.Initializer
import Persistence.Product
import Views.Template

import Control.Monad        (msum)
import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query', update')
import Happstack.Server     ( ServerPart, Method(GET, POST), Response, ok, lookRead, method, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.Maybe            (fromJust)

import           Logging.Global.TH   (debug, error, fatal, info, logv, warn)
import           Prelude             hiding (error)

logger = "Checkout"

viewCheckout  :: AcidState Store -> ServerPart Response
viewCheckout acid = do
    cartProds <- query' acid CartProducts
    -- subtotal  <- calculateSubtotal cartProds
    ok $ template "Checkout" [] (Mode "user") $ do
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
                        mapM_ viewCartProduct cartProds
                H.div ! A.class_ "payment-container" $ do
                    H.h2 $ "Order Summary"
                    H.div ! A.class_ "payment-subtotal" $ do
                        H.p $ do "Subtotal"
                        H.p $ do H.toHtml (calculateSubtotal cartProds)
                    H.div ! A.class_ "payment-shipping" $ do
                        H.p $ do "Shipping"
                        H.p $ do "$30"
                    H.div ! A.class_ "payment-total" $ do
                        H.p $ do "Estimated Total"
                        H.p $ do H.toHtml ((calculateSubtotal cartProds) + 30)
                    H.div ! A.class_ "purchase-btn-container" $ do
                        H.form ! A.enctype "multipart/form-data"
                                    ! A.method "POST"
                                    ! A.action "/purchase" $ H.button ! A.name "purchaseOrder" ! A.value "purchaseOrder" ! A.class_ "confirm-button" $ "Purchase"
  where 
      viewCartProduct CartProduct{..} =
            let url = (H.toValue $ "/product?id=" ++ show (unProductId cartProdId) ++ "&mode=user")
            in H.div ! A.class_ "shopping-cart-item" $ do
                    H.div ! A.class_ "shopping-cart-item-label" $ do
                        H.img ! A.src "images/product.png" ! A.alt "Product"
                        H.p $ H.a ! A.href url $ H.toHtml $ cartProdName
                    H.p ! A.class_ "shopping-cart-item-qty" $ do H.toHtml cartProdQty
                    H.div ! A.class_ "shopping-cart-item-price" $ do
                        H.p $ "$"
                        H.p $ do H.toHtml $ cartProdPrice


addToCart :: AcidState Store -> ServerPart Response
addToCart acid = do
    msum
        [
            do  method POST
                productid   <- ProductId <$> lookRead "id"
                cartProd <- update' acid $ AppendProduct productid

                let url = "/product?id=" ++ show (unProductId $ cartProdId cartProd) ++ "&mode=user"
                seeOther url (toResponse ())
        ]

homeURL :: String
homeURL = "/"

purchase :: AcidState Store -> ServerPart Response
purchase acid = do
    method POST
    update' acid EmptyCart
    seeOther homeURL (toResponse ())

calculateSubtotal :: [CartProduct] -> Int
calculateSubtotal cps = foldr (+) 0 $ map priceByQty cps

priceByQty :: CartProduct -> Int
priceByQty prod = (cartProdPrice prod) * (cartProdQty prod)
