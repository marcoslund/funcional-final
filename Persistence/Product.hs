{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Persistence.Product where

import Models.Store

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            ( Update, Query, makeAcidic)
import Data.IxSet           ( Indexable(..), (@=), Proxy(..), getOne )
import qualified Data.IxSet as IxSet
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))


initialStoreState :: Store
initialStoreState =
    Store { nextProductId  = ProductId 1
          , products       = empty
          , nextCategoryId = CategoryId 1
          , categories     = empty
          , nextUserId     = UserId 1
          , users          = empty
          }


newProduct :: UTCTime -> Update Store Product
newProduct pubDate =
    do b@Store{..} <- get
       let prod = Product { productId = nextProductId
                           , name = Text.empty
                           , brand = Text.empty
                           , description = Text.empty
                           , date   = pubDate
                           , status = Draft
                           , price = 0
                           , category = nextCategoryId
                           -- , tags   = []
                           }
       put $ b { nextProductId = succ nextProductId
               , products      = IxSet.insert prod products
               , nextCategoryId = nextCategoryId
               }
       return prod


-- | update the product in the database (indexed by ProductId)
updateProduct :: Product -> Update Store ()
updateProduct updatedProduct = do
  b@Store{..} <- get
  put $ b { products =
             IxSet.updateIx (productId updatedProduct) updatedProduct products
          }


productById :: ProductId -> Query Store (Maybe Product)
productById pid =
     do Store{..} <- ask
        return $ getOne $ products @= pid


productsByStatus :: Status -> Query Store [Product]
productsByStatus status = do
 Store{..} <- ask
 let products' =
       IxSet.toDescList (Proxy :: Proxy UTCTime) $
         products @= status
 return products'

productsByPrice :: (Proxy Price -> IxSet.IxSet Product -> [Product]) -> Query Store [Product]
productsByPrice func = do
 Store{..} <- ask
 let products' =
        func (Proxy :: Proxy Price) products
 return products'

productsByPriceAsc :: Query Store [Product]
productsByPriceAsc = productsByPrice IxSet.toAscList

productsByPriceDesc :: Query Store [Product]
productsByPriceDesc = productsByPrice IxSet.toDescList

productsByNameAsc :: String -> Query Store [Product]
productsByNameAsc text = do
 Store{..} <- ask
 let products' =
        IxSet.toAscList (Proxy :: Proxy Name) $
         products @= Text.pack text
 return products'

$(makeAcidic ''Store
  [ 'newProduct
  , 'updateProduct
  , 'productById
  , 'productsByStatus
  , 'productsByPriceAsc
  , 'productsByPriceDesc
  , 'productsByNameAsc
  ])