module Main where

import Happstack.Server (nullConf, simpleHTTP, ok, dir)
import Control.Monad (msum)

main :: IO ()
main = simpleHTTP nullConf $ msum [ dir "hello" $ ok "Hello world!",
                                    dir "goodbye" $ ok "Goodbye world!"
                                  ]
