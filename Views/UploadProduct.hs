{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.UploadProduct (viewNewProduct) where

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
import Happstack.Server     ( ServerPart, Method(POST, GET), Response, lookRead, looks, lookText, method, notFound, ok, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewNewProduct  :: AcidState Store -> ServerPart Response
viewNewProduct acid = do 
  msum
    [ do method GET
         mode <- looks "mode"
         case mode of
            ["admin"] -> do
                categories <- query' acid GetCategories
                ok $ template "Upload Product" [] (Mode "admin") $ do
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
                                    mapM_ viewCategoryOption categories
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
            _ -> 
                notFound $ template "Forbidden Access" [] (Mode "user") $
                    do "Forbidden Access"
                  
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
         seeOther ("/product?id=" ++ (show $ unProductId $ productId prod) ++ "&mode=admin") (toResponse ())
                  ]

      where lookText' = fmap toStrict . lookText
            viewCategoryOption Category{..} = 
                let categId = H.toValue $ show (unCategoryId categoryId)
                in H.option ! A.value categId $ H.toHtml categoryName
