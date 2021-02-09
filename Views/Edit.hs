{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Edit (edit, view, drafts) where

import Models.Store
import Persistence.Product
import Views.Template
import Views.Product

import Control.Applicative  ((<$>), optional)
import Control.Monad        (msum, mzero)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState)
import Data.Acid.Advanced   (update', query')
import Data.Text.Lazy       (toStrict)
import qualified Data.Text  as Text
import Data.Time            (getCurrentTime)
import Happstack.Server     ( ServerPart, Method(POST, GET), Response, lookRead, lookText, method, notFound, ok, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- | edit an existing product
edit :: AcidState Store -> ServerPart Response
edit acid = do
 pid   <- ProductId <$> lookRead "id"
 mMsg  <- optional $ lookText "msg"
 mProd <- query' acid (ProductById pid)
 case mProd of
  Nothing ->
   notFound $ template "no such product" [] $
                do "Could not find a product with id "
                   H.toHtml (unProductId pid)
  (Just p@(Product{..})) -> msum
    [ do method GET
         ok $ template "foo" [] $ do
          case mMsg of
            (Just msg) | msg == "saved" -> "Changes saved!"
            _ -> ""
          H.form ! A.enctype "multipart/form-data"
                  ! A.method "POST"
                  ! A.action (H.toValue $ "/edit?id=" ++
                                  (show $ unProductId pid)) $ do
            H.label "name" ! A.for "name"
            H.input ! A.type_ "text"
                    ! A.name "name"
                    ! A.id "name"
                    ! A.size "80"
                    ! A.value (H.toValue name)
            H.br
            H.label "brand" ! A.for "brand"
            H.input ! A.type_ "text"
                    ! A.name "brand"
                    ! A.id "brand"
                    ! A.size "40"
                    ! A.value (H.toValue brand)
            H.br
            -- H.label "tags" ! A.for "tags"
            -- H.input ! A.type_ "text"
            --        ! A.name "tags"
            --        ! A.id "tags"
            --        ! A.size "40"
            --        ! A.value (H.toValue $
            --                    Text.intercalate ", " tags)
            H.br
            H.label "description" ! A.for "description"
            H.br
            H.textarea ! A.cols "80"
                        ! A.rows "20"
                        ! A.name "body" $ H.toHtml description
            H.br
            H.button ! A.name "status"
                      ! A.value "publish" $ "publish"
            H.button ! A.name "status"
                      ! A.value "save"    $ "save as draft"
    , do method POST
         ttl   <- lookText' "name"
         athr  <- lookText' "brand"
         -- tgs   <- lookText' "tags"

         bdy   <- lookText' "description"
         now   <- liftIO $ getCurrentTime
         stts  <- do s <- lookText' "status"
                     case s of
                        "save"    -> return Draft
                        "publish" -> return Published
                        _         -> mzero
         let updatedProduct =
                 p { name  = ttl
                   , brand = athr
                   , description   = bdy
                   , date   = now
                   , status = stts
                   --, tags   =
                   --    map Text.strip $ Text.splitOn "," tgs
                   }
         update' acid (UpdateProduct updatedProduct)
         case status of
           Published ->
             seeOther ("/view?id=" ++ (show $ unProductId pid))
                      (toResponse ())
           Draft     ->
             seeOther ("/edit?msg=saved&id=" ++
                       (show $ unProductId pid))
                      (toResponse ())
                  ]

      where lookText' = fmap toStrict . lookText



-- | view a single product
view :: AcidState Store -> ServerPart Response
view acid =
    do pid <- ProductId <$> lookRead "id"
       mProd <- query' acid (ProductById pid)
       case mProd of
         Nothing ->
             notFound $ template "no such product" [] $
               do "Could not find a product with id "
                  H.toHtml (unProductId pid)
         --(Just p) ->
         --    ok $ template (name p) [] $ do
         --        (viewProduct p)



-- | show a list of all unpublished products
drafts :: AcidState Store -> ServerPart Response
drafts acid =
    do drafts <- query' acid (ProductsByStatus Draft)
       case drafts of
         [] -> ok $ template "drafts" [] $
               "You have no unpublished products at this time."
         _ ->
             ok $ template "home" [] $
                 H.ol $ mapM_ editDraftLink drafts
 where
  editDraftLink Product{..} =
    let url = (H.toValue $ "/edit?id=" ++ show (unProductId productId))
    in H.a ! A.href url $ H.toHtml name