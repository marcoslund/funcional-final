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
    Store { nextProductId = ProductId 1
          , products      = empty
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
                           -- , tags   = []
                           }
       put $ b { nextProductId = succ nextProductId
               , products      = IxSet.insert prod products
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


$(makeAcidic ''Store
  [ 'newProduct
  , 'updateProduct
  , 'productById
  , 'productsByStatus
  ])