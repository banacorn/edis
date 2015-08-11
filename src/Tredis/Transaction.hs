{-# LANGUAGE OverloadedStrings, GADTs #-}

module Tredis.Transaction where

import           Data.Typeable
import           Control.Applicative
import           Control.Monad.State
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize, encode, decode)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Database.Redis as Redis
import           Database.Redis (Redis, runRedis, Reply(..), sendRequest, Status)

type Tx = State TxState
type Key = ByteString

data TypeError
    = Undeclared Key
    | TypeMismatch Key TypeRep TypeRep

instance Show TypeError where
    show (Undeclared key) = "Undeclared: " ++ unpack key
    show (TypeMismatch key exp got) = "TypeMismatch: expect '" ++ show exp ++ "', got '" ++ show got ++ "'"

data Queued a = Queued ([Reply] -> Either Reply a)

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
    {   commands :: [Redis (Either Reply Status)]
    ,   typeTable :: Map Key TypeRep
    ,   typeError :: [(Int, TypeError)]
    ,   counter :: Int
    }

defaultTxState :: TxState
defaultTxState = TxState [] Map.empty [] 0

insertCmd :: Serialize a => [ByteString] -> Tx (Queued a)
insertCmd args = do
    state <- get
    let cmds = commands state
    let count = counter state
    put $ state { commands = sendRequest args : cmds
                , counter  = succ count
                }
    return $ Queued $ \replies -> decodeReply (replies !! count)

assertType :: Typeable a => Key -> a -> Tx ()
assertType key val = do
    state <- get
    let table = typeTable state
    put $ state { typeTable = Map.insert key (typeOf val) table }

removeType :: Key -> Tx ()
removeType key = do
    state <- get
    let table = typeTable state
    put $ state { typeTable = Map.delete key table }

assertError :: TypeError -> Tx ()
assertError err = do
    state <- get
    let errors = typeError state
    let count = counter state
    put $ state { typeError = (count, err) : errors }

lookupType :: Key -> Tx (Maybe TypeRep)
lookupType key = do
    state <- get
    let table = typeTable state
    return $ Map.lookup key table

checkType :: Key -> TypeRep -> Tx ()
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

decodeReply :: Serialize a => Reply -> Either Reply a
decodeReply (Bulk (Just raw)) = case decode raw of
    Left decodeErr -> Left $ Error (pack decodeErr)
    Right result   -> Right result
decodeReply others = Left others
-- decodeReply others = error $ "decode reply error " ++ show others

-- execute
execTx :: Serialize a => Tx (Queued a) -> Redis (Either Reply a)
execTx f = do

    -- issue MULTI
    multi

    -- extract states
    let (Queued queued, state) = runState f defaultTxState
    let (redisCommands) = reverse $ commands state

    -- run them all
    sequence redisCommands

    -- issue EXEC
    execResult <- exec
    case execResult of
        MultiBulk (Just replies) -> do
            return (queued replies)
        _ -> error "something went wrong"

    where
        multi :: Redis (Either Reply Status)
        multi = sendRequest ["MULTI"]
        exec :: Redis Reply
        exec = either id id <$> sendRequest ["EXEC"]

runTx :: Serialize a => Redis.Connection -> Tx (Queued a) -> IO (Either [(Int, TypeError)] (Either Reply a))
runTx conn f = runRedis conn $ do
    let state = execState f defaultTxState

    -- see if there's any type error
    let errors = typeError state
    if null errors
        then Right <$> execTx f         -- if none, then execute
        else Left  <$> return errors    -- else return the errors

-- re-export Redis shit
connect = Redis.connect
defaultConnectInfo = Redis.defaultConnectInfo
