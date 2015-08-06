{-# LANGUAGE OverloadedStrings, GADTs #-}

module Tredis.Transaction where

import Control.Applicative
import Control.Monad.State as State
import Data.ByteString hiding (map, unpack, pack, reverse)
import Data.ByteString.Char8 (pack, unpack)
import Database.Redis as Redis hiding (Queued, Set, decode)
import qualified Data.Serialize as S
import           Data.Serialize (Serialize)
import Data.Map as Map hiding (map)
import Data.List as List

type Tx = State TxState
type Key = ByteString

data Type = IntType | StrType
    deriving (Show, Eq)

data TypeError
    = Undeclared Key
    | TypeMismatch Key Type Type

instance Show TypeError where
    show (Undeclared key) = "Undeclared: " ++ unpack key
    show (TypeMismatch key exp got) = "TypeMismatch: expect '" ++ show exp ++ "', got '" ++ show got ++ "'"

data Command where
    Set :: Serialize a => Key -> a -> Command
    Append :: Key -> ByteString -> Command
    Get :: Key -> Command
    Del :: Key -> Command
    Incr :: Key -> Command

instance Show Command where
    show (_) = "Command"

--------------------------------------------------------------------------------
--  TxState
--------------------------------------------------------------------------------

data TxState = TxState
    {   commands :: [Command]
    ,   typeTable :: Map Key Type
    ,   typeError :: [(Int, TypeError)]
    ,   counter :: Int
    }   deriving (Show)

defaultTxState :: TxState
defaultTxState = TxState [] Map.empty [] 1

insertCmd :: Command -> Tx ()
insertCmd cmd = do
    state <- State.get
    let cmds = commands state
    let count = counter state
    State.put $ state { commands = cmd : cmds
                      , counter  = succ count
                      }

getAllCmds :: Tx () -> [Command]
getAllCmds f = reverse $ commands $ execState f defaultTxState

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
    let count = counter state
    State.put $ state { typeError = (count, err) : errors }

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

encode :: Serialize a => a -> ByteString
encode = S.encode

decode :: Serialize a => ByteString -> Either String a
decode = S.decode

--------------------------------------------------------------------------------
--  Tx
--------------------------------------------------------------------------------

toRedisCmd :: Command -> Redis (Either Reply Status)
toRedisCmd (Set key val) = sendRequest ["SET", key, encode val]
toRedisCmd (Get key)     = sendRequest ["GET", key]
toRedisCmd (Del key)     = sendRequest ["DEL", key]
toRedisCmd (Incr key)    = sendRequest ["INCR", key]
toRedisCmd (Append key val) = sendRequest ["APPEND", key, val]

decodeReply :: Serialize a => Reply -> [Either String a]
decodeReply (MultiBulk (Just xs)) = xs >>= decodeReply
decodeReply (Bulk (Just raw)) = return $ decode raw
decodeReply others = error $ "decode reply error " ++ show others

-- execute
execTx :: Tx () -> Redis Reply
execTx f = do
    multi                                           -- issue MULTI
    let commands = getAllCmds f                     -- get all commands
    let redisCommands = map toRedisCmd commands     -- make them Redis Commands
    sequence redisCommands                          -- run them all
    exec

    where
        multi :: Redis (Either Reply Status)
        multi = sendRequest ["MULTI"]
        exec :: Redis Reply
        exec = either id id <$> sendRequest ["EXEC"]

runTx :: Tx () -> Redis (Either [(Int, TypeError)] Reply)
runTx f = do
    let state = execState f defaultTxState

    -- see if there's any type error
    let errors = typeError state
    if List.null errors
        then Right <$> execTx f         -- if none, then execute
        else Left  <$> return errors    -- else return the errors
