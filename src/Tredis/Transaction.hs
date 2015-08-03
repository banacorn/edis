{-# LANGUAGE OverloadedStrings, GADTs #-}

module Tredis.Transaction where

import Data.Serialize as S
import Data.ByteString hiding (map, unpack)
import Data.ByteString.Char8 (unpack)
import Control.Applicative
-- import Control.Monad (void)
import Control.Monad.State as State
import Database.Redis as Redis hiding (Queued, Set)



type Tx = StateT TxState Redis
type Key = ByteString


data TypeError = TypeMismatch
    deriving (Show)

data TxState = TxState
    {   transactions :: [Transaction]
    ,   typeError :: [TypeError]
    }   deriving (Show)

data Transaction where
    Set :: (Show a, Serialize a) => Key -> a -> Transaction
    Get :: Key -> Transaction
    Incr :: Key -> Transaction

instance Show Transaction where
    show (Set k v) = "SET " ++ unpack k ++ " " ++ show v
    show (Get k) = "SET " ++ unpack k
    show (Incr k) = "SET " ++ unpack k

defaultTxState :: TxState
defaultTxState = TxState [] []

insertTx :: Transaction -> Tx ()
insertTx tx = do
    state <- State.get
    let txs = transactions state
    State.put $ state { transactions = tx : txs }

incr :: Key -> Tx ()
incr key = insertTx $ Incr key

get :: Key -> Tx ()
get key = insertTx $ Get key

set :: (Show a, Serialize a) => Key -> a -> Tx ()
set key val = insertTx $ Set key val

extractTx :: Tx () -> Redis [Transaction]
extractTx f = transactions <$> execStateT f defaultTxState

runTx :: Tx () -> Redis ()
runTx f = do
    txs <- extractTx f
    sequence_ (map toRedisTx txs)

toRedisTx :: Transaction -> Redis (Either Reply Status)
toRedisTx (Get key) = sendRequest ["GET", key]
toRedisTx (Set key val) = sendRequest ["SET", key, S.encode val]
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
