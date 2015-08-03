{-# LANGUAGE OverloadedStrings, GADTs #-}

module Tredis.Transaction where

-- import Data.Serialize (Serialize)
import Data.ByteString hiding (map)

import Control.Applicative
-- import Control.Monad (void)
import Control.Monad.State as State
import Database.Redis as Redis hiding (Queued, Set)



type Tx = StateT [Transaction] Redis
type Key = ByteString

data Transaction
    = Set Key ByteString
    | Get Key
    | Incr Key
    deriving (Show)

incr :: Key -> Tx ()
incr key = State.modify (Incr key :)

get :: Key -> Tx ()
get key = State.modify (Get key :)

set :: Key -> ByteString -> Tx ()
set key val = State.modify (Set key val :)

extractTx :: Tx () -> Redis [Transaction]
extractTx f = execStateT f []

runTx :: Tx () -> Redis ()
runTx f = do
    txs <- extractTx f
    sequence_ (map toRedisTx txs)

toRedisTx :: Transaction -> Redis (Either Reply Status)
toRedisTx (Get key) = sendRequest ["GET", key]
toRedisTx (Set key val) = sendRequest ["SET", key, val]
toRedisTx (Incr key) = sendRequest ["Incr", key]


execTransactions :: Tx () -> Redis ()
execTransactions f = do
    multi
    extractTx f >>= liftIO . print
    runTx f
    exec


    return ()

    where
        multi :: Redis (Either Reply Status)
        multi = sendRequest ["MULTI"]
        exec :: Redis Reply
        exec = either id id <$> sendRequest ["EXEC"]

        aaa :: Redis (Either Reply Integer)
        aaa = sendRequest ["incr", "a"]
