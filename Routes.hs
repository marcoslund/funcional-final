{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Routes (route) where

import Models.Store
import Views.Home
import Views.Categories
import Views.Category
import Views.Product
import Views.Checkout
import Views.UploadProduct
import Views.Edit

import Control.Monad        (msum)
import Data.Acid            (AcidState)
import Happstack.Server     ( ServerPart, Response, decodeBody, defaultBodyPolicy, dir, notFound, nullDir, toResponse, serveDirectory, Browsing( DisableBrowsing ))

-- | route incoming requests
route :: AcidState Store -> ServerPart Response
route acid =
    do decodeBody (defaultBodyPolicy "/tmp/" 0 1000000 1000000)
       msum [ dir "favicon.ico"         $ notFound (toResponse ())
            , dir "categories"          $ viewCategories acid
            , dir "products"            $ viewCategory acid
            , dir "product"             $ viewProduct acid
            , dir "checkout"            $ viewCheckout acid
            , dir "edit"                $ viewEditProduct acid
            , dir "new"                 $ viewNewProduct acid
            , dir "addtocart"           $ addToCart acid
            , dir "purchase"            $ purchase acid
            , dir "images"              $ serveDirectory DisableBrowsing ["index.html"] "images"
            , dir "fonts"               $ serveDirectory DisableBrowsing ["index.html"] "fonts"
            , nullDir                   >> home acid
            ]