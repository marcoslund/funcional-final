{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Persistence.Post where

import Models.Blog

import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            ( Update, Query, makeAcidic)
import Data.IxSet           ( Indexable(..), (@=), Proxy(..), getOne )
import qualified Data.IxSet as IxSet
import qualified Data.Text  as Text
import Data.Time            (UTCTime(..))


initialBlogState :: Blog
initialBlogState =
    Blog { nextPostId = PostId 1
         , posts      = empty
         }


newPost :: UTCTime -> Update Blog Post
newPost pubDate =
    do b@Blog{..} <- get
       let post = Post { postId = nextPostId
                       , title  = Text.empty
                       , author = Text.empty
                       , body   = Text.empty
                       , date   = pubDate
                       , status = Draft
                       , tags   = []
                       }
       put $ b { nextPostId = succ nextPostId
               , posts      = IxSet.insert post posts
               }
       return post


-- | update the post in the database (indexed by PostId)
updatePost :: Post -> Update Blog ()
updatePost updatedPost = do
  b@Blog{..} <- get
  put $ b { posts =
             IxSet.updateIx (postId updatedPost) updatedPost posts
          }


postById :: PostId -> Query Blog (Maybe Post)
postById pid =
     do Blog{..} <- ask
        return $ getOne $ posts @= pid


postsByStatus :: Status -> Query Blog [Post]
postsByStatus status = do
 Blog{..} <- ask
 let posts' =
       IxSet.toDescList (Proxy :: Proxy UTCTime) $
         posts @= status
 return posts'


$(makeAcidic ''Blog
  [ 'newPost
  , 'updatePost
  , 'postById
  , 'postsByStatus
  ])