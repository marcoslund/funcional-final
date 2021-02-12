{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Categories (viewCategories) where

import Models.Store
import Persistence.Initializer
import Persistence.Product
import Persistence.Category
import Views.Template
import Views.Product

import Data.Acid            (AcidState)
import Data.Acid.Advanced   (query', update')
import Happstack.Server     ( ServerPart, Response, ok, looks)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

viewCategories :: AcidState Store -> ServerPart Response
viewCategories acid =
    do categories <- query' acid GetCategories
       mode <- looks "mode"
       case mode of
            ["admin"] ->
                case categories of
                    [] -> do newCategs <- update' acid BuildCategories
                             ok $ template "Categories" [] (Mode "admin") $ do
                                    H.div ! A.class_ "container" $ do
                                        H.h1 ! A.class_ "title" $ "Categories"
                                        H.div ! A.class_ "items-container" $ do
                                            mapM_ (viewCategoryContainer "admin") newCategs
                    _  -> ok $ template "Categories" [] (Mode "admin") $ do
                                    H.div ! A.class_ "container" $ do
                                        H.h1 ! A.class_ "title" $ "Categories"
                                        H.div ! A.class_ "items-container" $ do
                                            mapM_ (viewCategoryContainer "admin") categories 
            _ ->
                case categories of
                    [] -> do newCategs <- update' acid BuildCategories
                             ok $ template "Categories" [] (Mode "user") $ do
                                    H.div ! A.class_ "container" $ do
                                        H.h1 ! A.class_ "title" $ "Categories"
                                        H.div ! A.class_ "items-container" $ do
                                            mapM_ (viewCategoryContainer "user") newCategs 
                    _  -> ok $ template "Categories" [] (Mode "user") $ do
                                    H.div ! A.class_ "container" $ do
                                        H.h1 ! A.class_ "title" $ "Categories"
                                        H.div ! A.class_ "items-container" $ do
                                            mapM_ (viewCategoryContainer "user") categories 
 where
    viewCategoryContainer mode Category{..} =
        let url = (H.toValue $ "/products?categoryId=" ++ show (unCategoryId categoryId) ++ "&mode=" ++ mode)
        in H.div ! A.class_ "category-container" $ H.a ! A.href url $ do
            H.h2 ! A.class_ "category-title" $ H.toHtml categoryName