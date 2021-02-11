{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.UploadProduct (viewUploadProduct, viewNewProduct) where

import Models.Store
import Persistence.Initializer
import Persistence.Product
import Views.Template

import Control.Applicative  ((<$>))
import Control.Monad        (msum)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState)
import Data.Acid.Advanced   (update', query')
import Data.Text.Lazy       (toStrict)
import Data.Text            (unpack)
import Data.Time            (getCurrentTime)
import Happstack.Server     ( ServerPart, Method(POST, GET), Response, lookRead, lookText, method, notFound, ok, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewNewProduct  :: AcidState Store -> ServerPart Response
viewNewProduct acid = do 
  msum
    [ do method GET
         ok $ template "Upload Product" [] $ do
          H.div ! A.class_ "container" $ do
            H.h1 ! A.class_ "title" $ "Upload Product"
            H.form  ! A.class_ "upload-product-form"
                    ! A.enctype "multipart/form-data"
                    ! A.method "POST"
                    ! A.action "/new" $ do
                H.div ! A.class_ "form-item" $ do
                    H.label "Product name" ! A.for "name"
                    H.input ! A.type_ "text"
                            ! A.name "name"
                            ! A.id "name"
                            ! A.size "80"
                H.div ! A.class_ "form-item" $ do
                    H.label "Brand" ! A.for "brand"
                    H.input ! A.type_ "text"
                            ! A.name "brand"
                            ! A.id "brand"
                            ! A.size "40"
                H.div ! A.class_ "form-item" $ do
                    H.label "Category" ! A.for "category"
                    H.select ! A.id "category" ! A.name "category" $ do
                        H.option ! A.value "1" $ "Food Market"
                        H.option ! A.value "2" $ "Electronics"
                        H.option ! A.value "3" $ "Clothing"
                H.div ! A.class_ "form-item" $ do
                    H.label "Price" ! A.for "price"
                    H.input ! A.type_ "number"
                            ! A.name "price"
                            ! A.id "price"
                            ! A.size "20"
                H.div ! A.class_ "form-item" $ do
                    H.label "Stock" ! A.for "stock"
                    H.input ! A.type_ "number"
                            ! A.name "stock"
                            ! A.id "stock"
                            ! A.size "20"
                H.div ! A.class_ "form-item" $ do
                    H.label "Description" ! A.for "description"
                    H.textarea ! A.cols "80"
                                ! A.rows "5"
                                ! A.name "description" $ ""
                H.div ! A.class_ "upload-product-btn-container" $ do
                    H.button ! A.name "upload" ! A.class_ "action-button"
                            ! A.value "upload" $ "Upload"
    , do method POST
         productName    <- lookText' "name"
         productBrand   <- lookText' "brand"
         productDesc    <- lookText' "description"
         now            <- liftIO $ getCurrentTime
         productCateg   <- lookText' "category"
         productPrice   <- lookText' "price"
         productStock   <- lookText' "stock"

         let newProduct =
                  NewProduct { newProdName = productName
                   , newProdBrand = productBrand
                   , newProdDesc = productDesc
                   , newProdDate = now
                   , newProdCategId = read (unpack productCateg)
                   , newProdPrice = read (unpack productPrice)
                   , newProdStock = read (unpack productStock)
                   }
         prod <- update' acid (CreateProduct newProduct now)
         seeOther ("/product?id=" ++ (show $ unProductId $ productId prod)) (toResponse ())
                  ]

      where lookText' = fmap toStrict . lookText

viewUploadProduct  :: AcidState Store -> ServerPart Response
viewUploadProduct acid = do
 pid   <- ProductId <$> lookRead "id"
 mProd <- query' acid (ProductById pid)
 case mProd of
  Nothing ->
   notFound $ template "Not found" [] $
                do "Could not find a product with ID "
                   H.toHtml (unProductId pid)
  (Just p@(Product{..})) -> msum
    [ do method GET
         ok $ template "Upload Product" [] $ do
          H.div ! A.class_ "container" $ do
            H.h1 ! A.class_ "title" $ "Upload Product"
            H.form  ! A.class_ "upload-product-form"
                    ! A.enctype "multipart/form-data"
                    ! A.method "POST"
                    ! A.action (H.toValue $ "/edit?id=" ++ (show $ unProductId pid)) $ do
                H.div ! A.class_ "form-item" $ do
                    H.label "Product name" ! A.for "name"
                    H.input ! A.type_ "text"
                            ! A.name "name"
                            ! A.id "name"
                            ! A.size "80"
                            ! A.value (H.toValue name)
                H.div ! A.class_ "form-item" $ do
                    H.label "Brand" ! A.for "brand"
                    H.input ! A.type_ "text"
                            ! A.name "brand"
                            ! A.id "brand"
                            ! A.size "40"
                            ! A.value (H.toValue brand)
                H.div ! A.class_ "form-item" $ do
                    H.label "Category" ! A.for "category"
                    H.select $ do --H.select ! A.id "category" $ [("1", "Opcion")]
                        H.option $ H.span $ "Food Market"
                        H.option $ H.span $ "Electronics"
                        H.option $ H.span $ "Clothing"
                H.div ! A.class_ "form-item" $ do
                    H.label "Price" ! A.for "price"
                    H.input ! A.type_ "number"
                            ! A.name "price"
                            ! A.id "price"
                            ! A.size "20"
                H.div ! A.class_ "form-item" $ do
                    H.label "Description" ! A.for "description"
                    H.textarea ! A.cols "80"
                                ! A.rows "5"
                                ! A.name "description" $ H.toHtml description
                H.div ! A.class_ "upload-product-btn-container" $ do
                    H.button ! A.name "upload" ! A.class_ "action-button"
                            ! A.value "upload" $ "Upload"
    , do method POST
         productName    <- lookText' "name"
         productBrand   <- lookText' "brand"
         productDesc    <- lookText' "description"
         now            <- liftIO $ getCurrentTime
         productPrice   <- lookText' "price"

         let updatedProduct =
                 p { name = productName
                   , brand = productBrand
                   , description = productDesc
                   , date = now
                   , price = read (unpack productPrice)
                   }
         update' acid (UpdateProduct updatedProduct)
         seeOther ("/product?id=" ++ (show $ unProductId pid)) (toResponse ())
                  ]

      where lookText' = fmap toStrict . lookText

--createProduct :: AcidState Store -> ServerPart Response
--createProduct acid = do
--  method POST
--  now <- liftIO $ getCurrentTime
--  prod <- update' acid (NewProduct now)
--  let url = "/edit?id=" ++ show (unProductId $ productId prod)
--  seeOther url (toResponse ())