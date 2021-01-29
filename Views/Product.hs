{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Product (productHtml) where

import Models.Store

import qualified Data.Text  as Text
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- | render a single product into an HTML fragment
productHtml  :: Product -> Html
productHtml (Product{..}) =
  H.div ! A.class_ "product" $ do
    H.h1 $ H.toHtml name
    H.div ! A.class_ "name" $
       do "brand: "
          H.toHtml brand
    H.div ! A.class_ "date"   $
       do "published: "
          H.toHtml (show date)
    -- H.div ! A.class_ "tags"   $
    --   do "tags: "
    --      H.toHtml (Text.intercalate ", " tags)
    H.div ! A.class_ "bdy" $ H.toHtml description
    H.div ! A.class_ "name" $
       do "price: "
          H.toHtml price
    H.div ! A.class_ "product-footer" $ do
     H.span $ H.a !
       A.href (H.toValue $ "/view?id=" ++
                show (unProductId productId)) $ "permalink"
     H.span $ " "
     H.span $ H.a !
       A.href (H.toValue $ "/edit?id=" ++
                show (unProductId productId)) $ "edit this product"