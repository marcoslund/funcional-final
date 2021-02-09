{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Persistence.User where

import Models.Store

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            ( Update, Query, makeAcidic)
import Data.IxSet           ( Indexable(..), (@=), Proxy(..), getOne )
import qualified Data.IxSet as IxSet
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))


newUser :: Update Store User
newUser =
    do b@Store{..} <- get
       let usr = User { userId = nextUserId
                      , email = Text.empty
                      , username = Text.empty
                      }
       put $ b { users      = IxSet.insert usr users
               , nextUserId = succ nextUserId
               }
       return usr


userById :: UserId -> Query Store (Maybe User)
userById uid =
     do Store{..} <- ask
        return $ getOne $ users @= uid

userByEmailAsc :: String -> Query Store [User]
userByEmailAsc email = do
 Store{..} <- ask
 let users' =
        IxSet.toAscList (Proxy :: Proxy Email) $
         users @= Text.pack email
 return users'
