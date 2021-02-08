{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Template (template) where

import Styles.Styles        (css)

import Data.Text            (Text)
import Happstack.Server     (Response, method, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- | HTML template that we use to render all the
--   pages on the site
template :: Text -> [Html] -> Html -> Response
template title headers body =
  toResponse $
    H.html $ do
      H.head $ do
        css
        H.title (H.toHtml title)
        H.meta ! A.httpEquiv "Content-Type"
               ! A.content "text/html;charset=utf-8"
        sequence_ headers
      H.body $ do
        H.ul ! A.id "menu" ! A.class_ "navbar" $ do
         H.li $ H.a ! A.href "/" $ do
           H.img ! A.src "images/logo.png" ! A.alt "Logo" ! A.class_ "logo"
         H.li ! A.class_ "navbar-link" $ H.a ! A.href "/categories" $ "Categories"
         H.li ! A.class_ "navbar-link" $ H.a ! A.href "/checkout" $ "Checkout"
         --H.li ! A.class_ "navbar-link" $ H.a ! A.href "/edit" $ "Upload Product"
         H.li ! A.class_ "navbar-link" $ H.form ! A.enctype "multipart/form-data"
                       ! A.method "POST"
                       ! A.action "/new" $ H.button $ "Upload Product"
        body