{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Persistence.Category where

import Models.Store

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            ( Update, Query, makeAcidic)
import Data.IxSet           ( Indexable(..), (@=), Proxy(..), getOne )
import qualified Data.IxSet as IxSet
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))


newCategory :: Update Store Category
newCategory =
    do b@Store{..} <- get
       let cat = Category { categoryId   = nextCategoryId
                          , categoryName = Text.empty
                          }
       put $ b { categories     = IxSet.insert cat categories
               , nextCategoryId = succ nextCategoryId
               }
       return cat

buildCategories :: Update Store [Category]
buildCategories =
    do b@Store{..} <- get
       let newCategs = [ Category { categoryId   = CategoryId 1
                                  , categoryName = "Food Market"
                                  }
                       , Category { categoryId   = CategoryId 2
                                  , categoryName = "Electronics"
                                  }
                       , Category { categoryId   = CategoryId 3
                                  , categoryName = "Clothing"
                                  }
                       , Category { categoryId   = CategoryId 4
                                  , categoryName = "Furniture"
                                  }
                       , Category { categoryId   = CategoryId 5
                                  , categoryName = "Vehicles"
                                  }
                       , Category { categoryId   = CategoryId 6
                                  , categoryName = "Hardware & Tools"
                                  }
                       , Category { categoryId   = CategoryId 7
                                  , categoryName = "Books"
                                  }
                       , Category { categoryId   = CategoryId 8
                                  , categoryName = "Beauty & Makeup"
                                  }
                       ]
       put $ b { categories     = insertN categories newCategs
               , nextCategoryId = incrementCategoryId nextCategoryId $ ownLength newCategs
               }
       return newCategs

insertN :: IxSet.IxSet Category -> [Category] -> IxSet.IxSet Category
insertN cset [] = cset
insertN cset (c:cs) = insertN (IxSet.insert c cset) cs

incrementCategoryId :: CategoryId -> Int -> CategoryId
incrementCategoryId id times = iterate succ id !! times

ownLength :: [a] -> Int
ownLength [] = 0
ownLength (_:xs) = 1 + ownLength xs

categoryById :: CategoryId -> Query Store (Maybe Category)
categoryById cid =
     do Store{..} <- ask
        return $ getOne $ categories @= cid

getCategories :: Query Store [Category]
getCategories = do
 Store{..} <- ask
 let categs' = IxSet.toAscList (Proxy :: Proxy CategoryId) $ categories
 return categs'

categoriesByNameAsc :: String -> Query Store [Category]
categoriesByNameAsc text = do
 Store{..} <- ask
 let categs' =
        IxSet.toAscList (Proxy :: Proxy CategoryName) $
         categories @= Text.pack text
 return categs'
