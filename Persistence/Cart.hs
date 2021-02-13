{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Persistence.Cart where

import Persistence.Product
import Models.Store

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            ( Update, Query, makeAcidic)
import Data.IxSet           ( Indexable(..), (@=), Proxy(..), getOne )
import qualified Data.IxSet as IxSet
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))
import Data.Maybe            (fromJust)

appendProduct :: ProductId -> Update Store CartProduct
appendProduct prodId =
    do s@Store{..} <- get
       let  prod = fromJust $ getOne $ products @= prodId
            cartProduct =
                  CartProduct {
                     cartProdId = prodId
                   , cartProdName = name prod
                   , cartProdPrice = price prod
                   , cartProdQty = 1
                   }
       put $ s { userCart = IxSet.insert cartProduct userCart
               }
       return cartProduct

cartProducts :: Query Store [CartProduct]
cartProducts = do
 Store{..} <- ask
 let products' =
        IxSet.toAscList (Proxy :: Proxy ProductId) userCart
 return products'

emptyCart :: Update Store ()
emptyCart =
  do s@Store{..} <- get
     put $ s { userCart = empty }
