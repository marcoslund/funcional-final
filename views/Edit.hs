{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    RecordWildCards, TemplateHaskell, TypeFamilies,
    OverloadedStrings #-}

module Views.Edit (edit, new, view, drafts) where

import Models.Blog
import Persistence.Post
import Views.Template
import Views.Post

import Control.Applicative  ((<$>), optional)
import Control.Monad        (msum, mzero)
import Control.Monad.Trans  (liftIO)
import Data.Acid            (AcidState)
import Data.Acid.Advanced   (update', query')
import Data.Text.Lazy       (toStrict)
import qualified Data.Text  as Text
import Data.Time            (getCurrentTime)
import Happstack.Server     ( ServerPart, Method(POST, GET), Response, lookRead, lookText, method, notFound, ok, seeOther, toResponse)
import           Text.Blaze.Html ((!), Html)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- | edit an existing blog post
edit :: AcidState Blog -> ServerPart Response
edit acid = do
 pid   <- PostId <$> lookRead "id"
 mMsg  <- optional $ lookText "msg"
 mPost <- query' acid (PostById pid)
 case mPost of
  Nothing ->
   notFound $ template "no such post" [] $
                do "Could not find a post with id "
                   H.toHtml (unPostId pid)
  (Just p@(Post{..})) -> msum
    [ do method GET
         ok $ template "foo" [] $ do
          case mMsg of
            (Just msg) | msg == "saved" -> "Changes saved!"
            _ -> ""
          H.form ! A.enctype "multipart/form-data"
                  ! A.method "POST"
                  ! A.action (H.toValue $ "/edit?id=" ++
                                  (show $ unPostId pid)) $ do
            H.label "title" ! A.for "title"
            H.input ! A.type_ "text"
                    ! A.name "title"
                    ! A.id "title"
                    ! A.size "80"
                    ! A.value (H.toValue title)
            H.br
            H.label "author" ! A.for "author"
            H.input ! A.type_ "text"
                    ! A.name "author"
                    ! A.id "author"
                    ! A.size "40"
                    ! A.value (H.toValue author)
            H.br
            H.label "tags" ! A.for "tags"
            H.input ! A.type_ "text"
                    ! A.name "tags"
                    ! A.id "tags"
                    ! A.size "40"
                    ! A.value (H.toValue $
                                Text.intercalate ", " tags)
            H.br
            H.label "body" ! A.for "body"
            H.br
            H.textarea ! A.cols "80"
                        ! A.rows "20"
                        ! A.name "body" $ H.toHtml body
            H.br
            H.button ! A.name "status"
                      ! A.value "publish" $ "publish"
            H.button ! A.name "status"
                      ! A.value "save"    $ "save as draft"
    , do method POST
         ttl   <- lookText' "title"
         athr  <- lookText' "author"
         tgs   <- lookText' "tags"

         bdy   <- lookText' "body"
         now   <- liftIO $ getCurrentTime
         stts  <- do s <- lookText' "status"
                     case s of
                        "save"    -> return Draft
                        "publish" -> return Published
                        _         -> mzero
         let updatedPost =
                 p { title  = ttl
                   , author = athr
                   , body   = bdy
                   , date   = now
                   , status = stts
                   , tags   =
                       map Text.strip $ Text.splitOn "," tgs
                   }
         update' acid (UpdatePost updatedPost)
         case status of
           Published ->
             seeOther ("/view?id=" ++ (show $ unPostId pid))
                      (toResponse ())
           Draft     ->
             seeOther ("/edit?msg=saved&id=" ++
                       (show $ unPostId pid))
                      (toResponse ())
                  ]

      where lookText' = fmap toStrict . lookText


-- | create a new blog post in the database,
--   and then redirect to /edit
new :: AcidState Blog -> ServerPart Response
new acid = do
  method POST
  now <- liftIO $ getCurrentTime
  post <- update' acid (NewPost now)
  let url = "/edit?id=" ++ show (unPostId $ postId post)
  seeOther url (toResponse ())



-- | view a single blog post
view :: AcidState Blog -> ServerPart Response
view acid =
    do pid <- PostId <$> lookRead "id"
       mPost <- query' acid (PostById pid)
       case mPost of
         Nothing ->
             notFound $ template "no such post" [] $
               do "Could not find a post with id "
                  H.toHtml (unPostId pid)
         (Just p) ->
             ok $ template (title p) [] $ do
                 (postHtml p)



-- | show a list of all unpublished blog posts
drafts :: AcidState Blog -> ServerPart Response
drafts acid =
    do drafts <- query' acid (PostsByStatus Draft)
       case drafts of
         [] -> ok $ template "drafts" [] $
               "You have no unpublished posts at this time."
         _ ->
             ok $ template "home" [] $
                 H.ol $ mapM_ editDraftLink drafts
 where
  editDraftLink Post{..} =
    let url = (H.toValue $ "/edit?id=" ++ show (unPostId postId))
    in H.a ! A.href url $ H.toHtml title