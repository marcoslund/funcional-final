{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Edit (viewEditProduct) where

import Models.Store
import Persistence.Initializer
import Persistence.Product
import Views.Template
import Views.Product

import Control.Applicative  ((<$>), optional)
import Control.Monad        (msum, mzero)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState)
import Data.Acid.Advanced   (update', query')
import Data.Text.Lazy       (toStrict)
import Data.Text            (unpack)
import qualified Data.Text  as Text
import Data.Time            (getCurrentTime)
import Happstack.Server     ( ServerPart, Method(POST, GET), Response, lookRead, looks, lookText, method, notFound, ok, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewEditProduct  :: AcidState Store -> ServerPart Response
viewEditProduct acid = do
 pid   <- ProductId <$> lookRead "id"
 mProd <- query' acid (ProductById pid)
 case mProd of
  Nothing ->
   notFound $ template "Not found" [] (Mode "admin") $
                do "Could not find a product with ID "
                   H.toHtml (unProductId pid)
  (Just p@(Product{..})) -> msum
    [ do method GET
         mode <- looks "mode"
         case mode of
            ["admin"] -> do
                categories <- query' acid GetCategories
                ok $ template "Edit Product" [] (Mode "admin") $ do
                    H.div ! A.class_ "container" $ do
                        H.h1 ! A.class_ "title" $ "Edit Product"
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
                                H.select ! A.id "category" ! A.name "category" $ do
                                    mapM_ viewCategoryOption categories
                            H.div ! A.class_ "form-item" $ do
                                H.label "Price" ! A.for "price"
                                H.input ! A.type_ "number"
                                        ! A.name "price"
                                        ! A.id "price"
                                        ! A.size "20"
                                        ! A.value (H.toValue price)
                            H.div ! A.class_ "form-item" $ do
                                H.label "Stock" ! A.for "stock"
                                H.input ! A.type_ "number"
                                        ! A.name "stock"
                                        ! A.id "stock"
                                        ! A.size "20"
                                        ! A.value (H.toValue stock)
                            H.div ! A.class_ "form-item" $ do
                                H.label "Description" ! A.for "description"
                                H.textarea ! A.cols "80"
                                            ! A.rows "5"
                                            ! A.name "description" $ H.toHtml description
                            H.div ! A.class_ "upload-product-btn-container" $ do
                                H.button ! A.name "save" ! A.class_ "action-button"
                                        ! A.value "save" $ "Save changes"
            _ -> 
                notFound $ template "Forbidden Access" [] (Mode "user") $
                    do "Forbidden Access"

    , do method POST
         productName    <- lookText' "name"
         productBrand   <- lookText' "brand"
         productDesc    <- lookText' "description"
         productCateg   <- lookText' "category"
         now            <- liftIO $ getCurrentTime
         productPrice   <- lookText' "price"
         productStock   <- lookText' "stock"

         let updatedProduct =
                 Product { productId = pid
                   , name = productName
                   , brand = productBrand
                   , description = productDesc
                   , date = now
                   , status = Draft
                   , price = read (unpack productPrice)
                   , category = CategoryId (read (unpack productCateg))
                   , stock = read (unpack productStock)
                   }
         update' acid (UpdateProduct updatedProduct)
         seeOther ("/product?id=" ++ (show $ unProductId pid)) (toResponse ())
                  ]

      where lookText' = fmap toStrict . lookText
            viewCategoryOption Category{..} = 
                let categId = H.toValue $ show (unCategoryId categoryId)
                in H.option ! A.value categId $ H.toHtml categoryName