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
      [ "@font-face { font-family: 'VarelaRound'; src: url('/fonts/VarelaRound-Regular.ttf'); }"
      , "body { color: #555; padding: 0; margin: 0; font-family: 'VarelaRound'; }"
      , "ul { list-style-type: none; }"
      , "ol { list-style-type: none; }"
      , "a { text-decoration: none; }"
      , "h1 { font-size: 1.5em; color: #555; margin: 0; }"
      , ".name { color: #aaa; }"
      , ".date { color: #aaa; }"
      , ".tags { color: #aaa; }"
      , ".product { border-bottom: 1px dotted #aaa; margin-top: 1em; }"
      , ".bdy  { color: #555; margin-top: 1em; }"
      , ".product-footer { margin-top: 1em; margin-bottom: 1em; }"
      , "label { display: inline-block; width: 3em; }"
      , "#menu { margin: 0; padding: 0; }"
      , "#menu form { display: inline; margin-left: 1em; }"
      , ".navbar { height: 75px; background-color: #3d4857; box-shadow: 0 2px 4px 0 rgba(0,0,0,.3); position: sticky; top: 0; width: 100%; display: flex; font-size: 25px; font-family: 'VarelaRound'; }"
      , ".navbar li { margin-left: 1.5em; display: flex; justify-content: center; align-items: center; }"
      , ".navbar-link { min-width: 150px; }"
      , ".navbar-link a { color: white; }"
      , ".navbar-link a:hover { color: #DDD; }"
      , ".logo { height: 55px; width: 55px; }"
      , ".home-img { width: 100%; height: 500px; background-image: linear-gradient(rgba(0, 0, 0, 0.4), rgba(0, 0, 0, 0.4)), url('images/home.jpg'); background-repeat: no-repeat; background-size: cover; }"
      , ".home-img-container { width: 60%; height: 100%; margin: auto; padding: 100px 0; box-sizing: border-box; text-align: center; display: flex; justify-content: center; align-items: center; }"
      , ".home-img h1 { font-size: 4em; color: white; }"
      , ".container { width: 90%; margin: 20px auto; }"
      , ".title { font-size: 2.5em; color: #3d4857; border-bottom: 3px #3d4857 solid; padding-bottom: 10px; }"
      , ".items-container { display: flex; flex-wrap: wrap; justify-content: center; margin-top: 15px; }"
      , ".category-container { width: 45%; height: 100px; border: 3px #1d232a solid; margin: 15px 15px; box-sizing: border-box; border-radius: 10px; background-color: #3d4857; padding: 10px; }"
      , ".category-container:hover { background-color: #C7511F; }"
      , ".category-container a { width: 100%; }"
      , ".category-title {font-size: 1.75em; color: white; text-align: center; }"
      , ".product-container { width: 100%; height: 150px; border-bottom: 2px #aaa solid; box-sizing: border-box; padding: 10px; display: flex; }"
      , ".product-container img { width: 125px; height}"
      , ".category-product-details { display: flex; width: 100%; padding: 10px; flex-flow: column; }"
      , ".category-product-details h2 { font-size: 1.75em; margin: 0; padding-bottom: 10px; }"
      , ".category-product-details a { color: #0F1111; }"
      , ".category-product-details a:hover { color: #C7511F; }"
      , ".category-product-details p { color: black; margin: 0; font-size: 1.25em; }"
      ]
 in H.style ! A.type_ "text/css" $ H.toHtml s