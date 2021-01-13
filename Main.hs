{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, TypeSynonymInstances,
    QuasiQuotes, OverloadedStrings, GeneralizedNewtypeDeriving,
    TemplateHaskell, RecordWildCards #-}

module Main where

import Control.Applicative
import Control.Applicative.Indexed
    (IndexedFunctor(..), IndexedApplicative(..))
import Control.Exception           ( bracket )
import Control.Monad               (msum)
import Control.Monad.Reader        ( ask )
import Control.Monad.State         ( get, put )
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.Data                   ( Data, Typeable )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy    as Lazy
import qualified Data.Text         as Strict
import Happstack.Server
import Happstack.Server.XMLGenT    ()
import Happstack.Server.HSP.HTML   ()
import HSP
import HSP.Monad                   (HSPT(..))
import Language.Haskell.HSX.QQ     (hsx)
import Text.Reform
    ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
    , (<++), commonFormErrorStr, decimal, prove
    , transformEither, transform )
import Text.Reform.Happstack
import Text.Reform.HSP.Text

type AppT m  = XMLGenT (HSPT XML (ServerPartT m))
type AppT' m = HSPT XML (ServerPartT m)

appTemplate :: ( Functor m, Monad m
               , EmbedAsChild (AppT' m) headers
               , EmbedAsChild (AppT' m) body
               ) =>
               Text    -- ^ contents of <title> tag
            -> headers -- ^ extra content for <head> tag.
                       --   use () for nothing
            -> body    -- ^ contents of <body> tag
            -> AppT m Response
appTemplate title headers body =
  toResponse <$> [hsx|
    <html>
     <head>
      <title><% title %></title>
      <% headers %>
     </head>
     <body>
      <% body %>
     </body>
    </html>
    |]

--newtype Form m input error view proof a = Form { ... }
type SimpleForm = Form (AppT IO) [Input] AppError [AppT IO XML] ()

data AppError
    = Required
    | NotANatural String
    | AppCFE (CommonFormError [Input])
      deriving Show

instance (Functor m, Monad m) =>
    EmbedAsChild (AppT' m) AppError where
  asChild Required          =
    asChild $ "required"

  asChild (NotANatural str) =
    asChild $ "Could not decode as a positive integer: " ++ str

  asChild (AppCFE cfe)      =
     asChild $ commonFormErrorStr show cfe

instance (Functor m, Monad m) =>
         EmbedAsChild (AppT' m) Strict.Text where
    asChild t = asChild (Lazy.fromStrict t)

instance (Functor m, Monad m) =>
         EmbedAsAttr (AppT' m) (Attr Text Strict.Text) where
    asAttr (n := v) = asAttr (n := Lazy.fromStrict v)

instance FormError AppError where
    type ErrorInputType AppError = [Input]
    commonFormError = AppCFE

data Message = Message
    { name    :: Strict.Text -- ^ the author's name
    , title   :: Strict.Text -- ^ the message title
    , message :: Strict.Text -- ^ contents of the message
    } deriving (Eq, Ord, Read, Show)

renderMessage :: ( Functor m
                 , Monad m
                 , EmbedAsChild (AppT' m) Strict.Text) =>
                 Message -> AppT m XML
renderMessage msg =
   [hsx|
    <dl>
      <dt>name:</dt>    <dd><% name msg    %></dd>
      <dt>title:</dt>   <dd><% title msg   %></dd>
      <dt>message:</dt> <dd><% message msg %></dd>
    </dl>
   |]

postForm :: SimpleForm Message
postForm =
  Message
   <$> labelText "name:"             ++> inputText ""      <++ br
   <*> labelText "title: "           ++> inputText ""      <++ br
   <*> (labelText "message:" <++ br) ++> textarea 80 40 "" <++ br
   <*  inputSubmit "post"


data CounterState = CounterState { count :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CounterState)

initialCounterState :: CounterState
initialCounterState = CounterState 0

incCountBy :: Integer -> Update CounterState Integer
incCountBy n =
    do c@CounterState{..} <- get
       let newCount = count + n
       put $ c { count = newCount }
       return newCount

peekCount :: Query CounterState Integer
peekCount = count <$> ask

$(makeAcidic ''CounterState ['incCountBy, 'peekCount])

--postPage2 :: AppT IO Response
postPage2 =
 --dir "post2" $
  let action = ("/post2" :: Text) in
  appTemplate "post 2" () $[hsx|
   <% reform (form action) "post2" displayMsg Nothing postForm %>
  |]
 where
  displayMsg msg =
    appTemplate "Your Message" () $ renderMessage msg

handlers :: AcidState CounterState -> ServerPart Response
handlers acid = msum
  [ dir "peek" $ do
      c <- query' acid PeekCount
      ok $ toResponse $"peeked at the count and saw: " ++ show c
  , dir "post2" $ do 
      --c <- query' acid PeekCount 
      unHSPT $ unXMLGenT $ postPage2 
  , do nullDir
       c <- update' acid (IncCountBy 1)
       ok $ toResponse $ "New count is: " ++ show c
  ]

main :: IO ()
main =
  bracket (openLocalState initialCounterState)
          (createCheckpointAndClose)
           (\acid ->
               simpleHTTP nullConf (handlers acid))

{- main :: IO ()
main = simpleHTTP nullConf $ unHSPT $ unXMLGenT $ do
 decodeBody (defaultBodyPolicy "/tmp/" 0 10000 10000)
 msum [ postPage2
      , do nullDir
           appTemplate "forms" () $ [hsx|
            <ul>
             <li>
               <a href="/post2">
                 Simple Form (postPage2 implementation)
               </a>
             </li>
            </ul> |]
      ] -}