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


appendProduct :: ProductId -> Int -> Update Store Cart
appendProduct prod qty =
    do s@Store{..} <- get
       let prd  = CartProduct (prod, qty)
       put $ s { userCart = Cart { purchasedProducts = IxSet.insert prd $ purchasedProducts $ userCart }
               }
       return userCart
