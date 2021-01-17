{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Post (postHtml) where

import Models.Blog

import qualified Data.Text  as Text
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- | render a single blog post into an HTML fragment
postHtml  :: Post -> Html
postHtml (Post{..}) =
  H.div ! A.class_ "post" $ do
    H.h1 $ H.toHtml title
    H.div ! A.class_ "author" $
       do "author: "
          H.toHtml author
    H.div ! A.class_ "date"   $
       do "published: "
          H.toHtml (show date)
    H.div ! A.class_ "tags"   $
       do "tags: "
          H.toHtml (Text.intercalate ", " tags)
    H.div ! A.class_ "bdy" $ H.toHtml body
    H.div ! A.class_ "post-footer" $ do
     H.span $ H.a !
       A.href (H.toValue $ "/view?id=" ++
                show (unPostId postId)) $ "permalink"
     H.span $ " "
     H.span $ H.a !
       A.href (H.toValue $ "/edit?id=" ++
                show (unPostId postId)) $ "edit this post"