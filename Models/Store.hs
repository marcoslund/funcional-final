{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Models.Store where

import Data.Data            (Data, Typeable)
import Data.IxSet           ( Indexable(..), IxSet(..), ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        (base, deriveSafeCopy)
import Data.Text            (Text)
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))

------------------------------------------------

newtype ProductId = ProductId { unProductId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable)
$(deriveSafeCopy 0 'base ''ProductId)

------------------------------------------------

data Status =
    Draft
  | Published
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Status)

------------------------------------------------

newtype Name     = Name Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Name)

------------------------------------------------

newtype Brand    = Brand Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Brand)

------------------------------------------------

newtype Description = Description Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Description)

------------------------------------------------

newtype Price = Price Int
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Price)

------------------------------------------------

newtype Stock = Stock Int
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Stock)

------------------------------------------------

newtype CategoryId = CategoryId { unCategoryId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable)
$(deriveSafeCopy 0 'base ''CategoryId)

------------------------------------------------

newtype CategoryName     = CategoryName Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''CategoryName)

------------------------------------------------

newtype UserId = UserId { unUserId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable)
$(deriveSafeCopy 0 'base ''UserId)

------------------------------------------------

newtype Email = Email Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Email)

------------------------------------------------

newtype UserName = UserName Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserName)

------------------------------------------------

data Product = Product
    { productId   :: ProductId
    , name        :: Text
    , brand       :: Text
    , description :: Text
    , date        :: UTCTime
    , status      :: Status
    , price       :: Int
    , category    :: CategoryId
    , stock       :: Int
    --, tags    :: [Text]
    }
    deriving (Eq, Ord, Data, Typeable)
    
$(deriveSafeCopy 0 'base ''Product)

instance Indexable Product where
  empty = ixSet
    [ ixFun $ \pr -> [ productId pr ]
    , ixFun $ \pr -> [ Name  $ name pr  ]
    , ixFun $ \pr -> [ Brand $ brand pr ]
    , ixFun $ \pr -> [ status pr ]
    -- , ixFun $ \bp -> map Tag (tags bp)
    , ixFun $ (:[]) . date  -- point-free, just for variety
    , ixFun $ \pr -> [ Price $ price pr ]
    -- , ixFun $ \bp -> [ WordCount (length $ Text.words $ body bp) ]
    , ixFun $ \pr -> [ Stock $ stock pr ]
    ]

------------------------------------------------

data Category = Category
    { categoryId   :: CategoryId
    , categoryName :: Text
    }
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Category)

instance Indexable Category where
  empty = ixSet
    [ ixFun $ \cg -> [ categoryId cg ]
    , ixFun $ \cg -> [ CategoryName $ categoryName cg ]
    ]

------------------------------------------------

data User = User
    { userId    :: UserId
    , email     :: Text
    , username  :: Text
    }
    deriving (Eq, Ord, Data, Typeable)
    
$(deriveSafeCopy 0 'base ''User)

instance Indexable User where
  empty = ixSet
    [ ixFun $ \us -> [ userId us ]
    , ixFun $ \us -> [ Email  $ email us  ]
    , ixFun $ \us -> [ UserName $ username us ]
    ]

------------------------------------------------

data Store = Store
    { nextProductId  :: ProductId
    , products       :: IxSet Product
    , nextCategoryId :: CategoryId
    , categories     :: IxSet Category
    , nextUserId     :: UserId
    , users          :: IxSet User
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Store)
