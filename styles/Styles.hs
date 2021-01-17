{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}
    
module Styles.Styles (css) where

import qualified Data.Text  as Text
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- | CSS for our site
--
-- Normally this would live in an external .css file.
-- It is included inline here to keep the example
-- self-contained.
css :: Html
css =
 let s = Text.concat
      [ "body { color: #555; padding: 0; margin: 0; margin-left: 1em;}"
      , "ul { list-style-type: none; }"
      , "ol { list-style-type: none; }"
      , "h1 { font-size: 1.5em; color: #555; margin: 0; }"
      , ".author { color: #aaa; }"
      , ".date { color: #aaa; }"
      , ".tags { color: #aaa; }"
      , ".post { border-bottom: 1px dotted #aaa; margin-top: 1em; }"
      , ".bdy  { color: #555; margin-top: 1em; }"
      , ".post-footer { margin-top: 1em; margin-bottom: 1em; }"
      , "label { display: inline-block; width: 3em; }"
      , "#menu { margin: 0; padding: 0; margin-left: -1em;"
      ,         "border-bottom: 1px solid #aaa; }"
      , "#menu li { display: inline; margin-left: 1em; }"
      , "#menu form { display: inline; margin-left: 1em; }"
      ]
 in H.style ! A.type_ "text/css" $ H.toHtml s