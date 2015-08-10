{-# LANGUAGE OverloadedStrings, GADTs #-}

module Tredis.Transaction where

import Data.ByteString hiding (map, unpack, pack, reverse)
import Data.ByteString.Char8 (pack, unpack)
import Control.Applicative
import Control.Monad.State as State
import Database.Redis as Redis hiding (Queued, Set)
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

data Command
    = Set Key ByteString
    | Get Key
    | Del Key
    | Incr Key
    | Append Key ByteString
    deriving (Show)

data Queued a = Queued ([Reply] -> Either String a)

instance Functor Queued where
    fmap f (Queued g) = Queued (fmap f . g)

instance Applicative Queued where
    pure x                = Queued (const $ Right x)
    Queued f <*> Queued x = Queued $ \rs -> do
                                        f' <- f rs
                                        x' <- x rs
                                        return (f' x')

instance Monad Queued where
    return         = pure
    Queued x >>= f = Queued $ \rs -> do
                                x' <- x rs
                                let Queued f' = f x'
                                f' rs
                                
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

--------------------------------------------------------------------------------
--  Tx
--------------------------------------------------------------------------------

toRedisCmd :: Command -> Redis (Either Reply Status)
toRedisCmd (Set key val) = sendRequest ["SET", key, val]
toRedisCmd (Get key)     = sendRequest ["GET", key]
toRedisCmd (Del key)     = sendRequest ["DEL", key]
toRedisCmd (Incr key)    = sendRequest ["INCR", key]
toRedisCmd (Append key val) = sendRequest ["APPEND", key, val]

-- execute
execTx :: Tx () -> Redis Reply
execTx f = do
    multi                                           -- issue MULTI
    let commands = getAllCmds f                     -- get all commands
    let redisCommands = map toRedisCmd commands     -- make them Redis Commands
    sequence redisCommands                          -- run them all
    exec                                            -- issue EXEC

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
