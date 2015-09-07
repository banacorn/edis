{-# LANGUAGE OverloadedStrings  #-}

module Test where

import Data.Typeable
import Database.Redis
import Control.Monad.State (liftIO)

main :: IO ()
main = do

    conn <- connect defaultConnectInfo
    runRedis conn $ do
         set "n" "banana"
         incr "n"
         n <- get "n"
         liftIO $ print n
