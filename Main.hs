-- module Main where

-- import Happstack.Server (nullConf, simpleHTTP, ok, dir, path)
-- import Control.Monad (msum)

-- main :: IO ()
-- main = simpleHTTP nullConf $ msum [ dir "hello" $ ok "Hello world!",
--                                     dir "goodbye" $ path $ \s -> ok $ "Goodbye " ++ s
--                                   ]

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Main where

import Happstack.Server (Request(rqMethod), ServerPartT, askRq, nullConf, simpleHTTP, dir, path, ok)
import Control.Monad (msum)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import HSP.HTML4                   (renderAsHTML)
import HSP.Monad                   (HSPT(unHSPT))
import HSP.XML                     (XML, fromStringLit)
import HSP.XMLGenerator            (Attr(..), XMLGenT, XMLGen(..), EmbedAsAttr(..), EmbedAsChild(..), unXMLGenT)
import Language.Haskell.HSX.QQ     (hsx)

html :: (Functor m, Monad m) => XMLGenT (HSPT XML m) XML
html = [hsx| <p class="some">I haz a paragraph!</p> |]
hello = T.putStrLn $ renderAsHTML (unHSPT $ unXMLGenT $ html)

main :: IO ()
main = simpleHTTP nullConf $ msum [ dir "hello" $ ok "Hello world!",
                                     dir "goodbye" $ path $ \s -> ok $ "Goodbye " ++ s
                                   ]

