{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Persistence.Cart where

import Models.Store

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            ( Update, Query, makeAcidic)
import Data.IxSet           ( Indexable(..), (@=), Proxy(..), getOne )
import qualified Data.IxSet as IxSet
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))

appendProduct :: ProductId -> Update Store CartProduct
appendProduct prod =
    do s@Store{..} <- get
       let cartProduct =
                  CartProduct { cartProdId = prod
                   , cartProdQty = 1
                   }
       put $ s { userCart = IxSet.insert cartProduct userCart
               }
       return cartProduct
