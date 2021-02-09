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
      , "button { font-size: 1em; font-family: 'VarelaRound'; padding: 10px; border: none; cursor: pointer; }"
      , "button:focus { outline: none; }"
      , "ul { list-style-type: none; }"
      , "ol { list-style-type: none; }"
      , "a { text-decoration: none; color: inherit; }"
      , "h1 { font-size: 1.5em; color: #555; margin: 0; }"
      , "p { margin: 0; }"
      , "textarea { vertical-align: top; font-family: 'VarelaRound'; }"
      , "input { font-family: 'VarelaRound'; }"
      , "select { font-family: 'VarelaRound'; }"
      , ".name { color: #aaa; }"
      , ".date { color: #aaa; }"
      , ".tags { color: #aaa; }"
      , ".product { border-bottom: 1px dotted #aaa; margin-top: 1em; }"
      , ".bdy  { color: #555; margin-top: 1em; }"
      , ".product-footer { margin-top: 1em; margin-bottom: 1em; }"
      , "label { display: inline-block; width: 3em; }"
      , "#menu { margin: 0; padding: 0; }"
      , "#menu form { margin: 0; }"
      , ".navbar { height: 75px; background-color: #3d4857; box-shadow: 0 2px 4px 0 rgba(0,0,0,.3); position: sticky; top: 0; width: 100%; display: flex; font-size: 25px; font-family: 'VarelaRound'; }"
      , ".navbar li { margin-left: 1.5em; display: flex; justify-content: center; align-items: center; }"
      , ".navbar-link { min-width: 150px; }"
      , ".navbar-link a { color: white; }"
      , ".navbar-link a:hover { color: #C7511F; }"
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
      , ".product-container img { width: 125px; height: 125px; }"
      , ".category-product-details { display: flex; width: 100%; padding: 10px; flex-flow: column; }"
      , ".category-product-details h2 { font-size: 1.75em; margin: 0; padding-bottom: 10px; }"
      , ".category-product-details a { color: #0F1111; }"
      , ".category-product-details a:hover { color: #C7511F; }"
      , ".category-product-details p { color: black; margin: 0; font-size: 1.25em; }"
      , ".profile-container { margin: 20px; display: flex; }"
      , ".product-image { width: 600px; height: 600px; }"
      , ".product-details { display: flex; flex-flow: column; width: 100%; margin: 20px; color: #0F1111; font-size: 1.25em; }"
      , ".product-details h2 { font-size: 2em; color: black; border-bottom: 2px #ddd solid; padding-bottom: 10px; }"
      , ".product-name { margin: 0 0 15px; }"
      , ".detail-container { display: flex; flex-flow: row; align-items: center; }"
      , ".detail-container p { margin-top: 10px; margin-bottom: 10px; }"
      , ".detail-title { font-weight: bold; margin-right: 10px; }"
      , ".detail-price { font-size: 1.25em; color: #B12704; }"
      , ".detail-container-column { display: flex; flex-flow: column; }"
      , ".detail-container-column p { margin-top: 10px; margin-bottom: 10px; }"
      , ".detail-indent { margin: 0 20px !important; }"
      , ".white-space-pre { white-space: pre; }"
      , ".action-button { background-color: #dca312; color: white; }"
      , ".action-button:hover { background-color: #c49210; }"
      , ".add-cart-btn-container { margin-top: 20px; }"
      , ".back-btn { margin: 20px 20px 10px; background-color: #3d4857; color: white; }"
      , ".back-btn a { color: white; }"
      , ".back-btn:hover { background-color: #282f39; }"
      , ".checkout-container { display: flex; color: black; margin-top: 20px; }"
      , ".shopping-cart-container { flex-basis: 65%; margin: 5px 20px 0; }"
      , ".shopping-cart-container h2 { border-bottom: 2px #ccc solid; padding-bottom: 15px; margin: 10px 0; }"
      , ".shopping-cart-headers { display: flex; justify-content: space-between; font-weight: bold; margin-bottom: 20px; }"
      , ".shopping-cart-items { font-size: 1.25em; }"
      , ".shopping-cart-item { display: flex; justify-content: space-between; margin: 10px 0; padding: 5px 0; border-bottom: 1px #ddd solid; }"
      , ".shopping-cart-item-label { display: flex; width: 65%; }"
      , ".shopping-cart-item-label img { height: 150px; margin-right: 20px; }"
      , ".shopping-cart-item-qty { width: 10%; text-align: center; }"
      , ".shopping-cart-item-price { width: 10%; text-align: right; font-weight: bold; }"
      , ".payment-container { flex-basis: 30%; height: fit-content; background-color: #eee; margin: 20px; padding: 20px; }"
      , ".payment-container h2 { margin: 0; padding-bottom: 10px; }"
      , ".payment-subtotal { display: flex; justify-content: space-between; padding: 5px 0; }"
      , ".payment-shipping { display: flex; justify-content: space-between; padding: 5px 0; }"
      , ".payment-total { display: flex; justify-content: space-between; padding: 10px 0 0; border-top: 2px #ccc solid; margin-top: 10px; }"
      , ".payment-total:last-child { font-weight: bold; }"
      , ".purchase-btn-container { margin-top: 20px; display: flex; justify-content: center; }"
      , ".purchase-btn-container button { width: 40%; }"
      , ".confirm-button { background-color: #52948b; color: white; }"
      , ".confirm-button:hover { background-color: #396761; }"
      , ".upload-product-form { margin: 30px; display: flex; flex-flow: column; }"
      , ".upload-product-form label { width: 10em; }"
      , ".form-item { padding: 10px 0; }"
      , ".upload-product-btn-container { margin-top: 20px; }"
      ]
 in H.style ! A.type_ "text/css" $ H.toHtml s