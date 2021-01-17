{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Models.Blog where

import Data.Data            (Data, Typeable)
import Data.IxSet           ( Indexable(..), IxSet(..), ixFun, ixSet )
import qualified Data.IxSet as IxSet
import Data.SafeCopy        (base, deriveSafeCopy)
import Data.Text            (Text)
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))

------------------------------------------------

newtype PostId = PostId { unPostId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable)
$(deriveSafeCopy 0 'base ''PostId)

------------------------------------------------

data Status =
    Draft
  | Published
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Status)

------------------------------------------------

newtype Title     = Title Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Title)

------------------------------------------------

newtype Author    = Author Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Author)

------------------------------------------------

newtype Tag       = Tag Text
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''Tag)

------------------------------------------------

newtype WordCount = WordCount Int
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''WordCount)

------------------------------------------------

data Post = Post
    { postId  :: PostId
    , title   :: Text
    , author  :: Text
    , body    :: Text
    , date    :: UTCTime
    , status  :: Status
    , tags    :: [Text]
    }
    deriving (Eq, Ord, Data, Typeable)
    
$(deriveSafeCopy 0 'base ''Post)

instance Indexable Post where
  empty = ixSet
    [ ixFun $ \bp -> [ postId bp ]
    , ixFun $ \bp -> [ Title  $ title bp  ]
    , ixFun $ \bp -> [ Author $ author bp ]
    , ixFun $ \bp -> [ status bp ]
    , ixFun $ \bp -> map Tag (tags bp)
    , ixFun $ (:[]) . date  -- point-free, just for variety
    , ixFun $ \bp -> [ WordCount (length $ Text.words $ body bp) ]
    ]

------------------------------------------------

data Blog = Blog
    { nextPostId :: PostId
    , posts      :: IxSet Post
    }
    deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Blog)