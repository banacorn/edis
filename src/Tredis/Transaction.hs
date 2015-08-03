{-# LANGUAGE OverloadedStrings, GADTs #-}

module Tredis.Transaction where

import Data.ByteString hiding (map, unpack, pack, reverse)
import Data.ByteString.Char8 (pack, unpack)
import Control.Applicative
import Control.Monad.State as State
import Database.Redis as Redis hiding (Queued, Set)
import Data.Map as Map hiding (map)
import Data.List as List

type Tx = StateT TxState Redis
type Key = ByteString

data Type = IntType | StrType
    deriving (Show, Eq)

data TypeError
    = Undeclared Key
    | TypeMismatch Key Type Type

instance Show TypeError where
    show (Undeclared key) = "Undeclared: " ++ unpack key
    show (TypeMismatch key exp got) = "TypeMismatch: expect '" ++ show exp ++ "', got '" ++ show got ++ "'"

data Transaction
    = Set Key ByteString
    | Get Key
    | Del Key
    | Incr Key
    deriving (Show)

--------------------------------------------------------------------------------
--  TxState
--------------------------------------------------------------------------------

data TxState = TxState
    {   transactions :: [Transaction]
    ,   typeTable :: Map Key Type
    ,   typeError :: [TypeError]
    }   deriving (Show)

defaultTxState :: TxState
defaultTxState = TxState [] Map.empty []

insertTx :: Transaction -> Tx ()
insertTx tx = do
    state <- State.get
    let txs = transactions state
    State.put $ state { transactions = tx : txs }

getAllTx :: Tx () -> Redis [Transaction]
getAllTx f = reverse . transactions <$> execStateT f defaultTxState

assertType :: Key -> Type -> Tx ()
assertType key typ = do
    state <- State.get
    let table = typeTable state
    State.put $ state { typeTable = Map.insert key typ table }

removeType :: Key -> Tx ()
removeType key = do
    state <- State.get
    let table = typeTable state
    State.put $ state { typeTable = Map.delete key table }

assertError :: TypeError -> Tx ()
assertError err = do
    state <- State.get
    let errors = typeError state
    State.put $ state { typeError = err : errors }

lookupType :: Key -> Tx (Maybe Type)
lookupType key = do
    state <- State.get
    let table = typeTable state
    return $ Map.lookup key table

checkType :: Key -> Type -> Tx ()
checkType key typ = do
    result <- lookupType key
    case result of
        Nothing -> assertError (Undeclared key)
        Just ty -> case ty == typ of
            True -> return ()
            False -> assertError (TypeMismatch key typ ty)

--------------------------------------------------------------------------------
--  Tx
--------------------------------------------------------------------------------

toRedisTx :: Transaction -> Redis (Either Reply Status)
toRedisTx (Set key val) = sendRequest ["SET", key, val]
toRedisTx (Get key)     = sendRequest ["GET", key]
toRedisTx (Del key)     = sendRequest ["DEL", key]
toRedisTx (Incr key)    = sendRequest ["INCR", key]

-- execute
execTx :: Tx () -> Redis Reply
execTx f = do
    multi                                           -- issue MULTI
    transactions <- getAllTx f                      -- get all transactions
    let redisCommands = map toRedisTx transactions  -- make them Redis Commands
    sequence redisCommands                          -- run them all
    exec                                            -- issue EXEC

    where
        multi :: Redis (Either Reply Status)
        multi = sendRequest ["MULTI"]
        exec :: Redis Reply
        exec = either id id <$> sendRequest ["EXEC"]

runTx :: Tx () -> Redis (Either [TypeError] Reply)
runTx f = do
    state <- execStateT f defaultTxState

    -- see if there's any type error
    let errors = typeError state
    if List.null errors
        then Right <$> execTx f         -- if none, then execute
        else Left  <$> return errors    -- else return the errors


--------------------------------------------------------------------------------
--  Commands
--------------------------------------------------------------------------------

declare :: Key -> Type -> Tx ()
declare = assertType

get :: Key -> Tx ()
get key = insertTx $ Get key

set :: Key -> ByteString -> Tx ()
set key val = do
    assertType key StrType
    insertTx $ Set key val

del :: Key -> Tx ()
del key = do
    removeType key
    insertTx $ Del key

setInt :: Key -> Integer -> Tx ()
setInt key val = do
    assertType key IntType
    insertTx $ Set key (pack (show val))

incr :: Key -> Tx ()
incr key = do
    checkType key IntType
    insertTx $ Incr key
