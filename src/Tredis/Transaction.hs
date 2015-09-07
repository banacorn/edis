{-# LANGUAGE OverloadedStrings #-}

module Tredis.Transaction where

import           Tredis.Serialize
import           Tredis.Type

import           Control.Applicative ((<$>))
import           Control.Monad.State
import           Data.ByteString.Char8 (pack)
import qualified Data.Map as Map
import qualified Database.Redis as Redis
import           Database.Redis (Redis, runRedis, Reply(..), sendRequest)

--------------------------------------------------------------------------------
--  TxState manipulation
--------------------------------------------------------------------------------

defaultTxState :: TxState
defaultTxState = TxState [] Map.empty [] 0

-- commands
insertCommand :: Command -> Tx' Int
insertCommand cmd = do
    states <- get
    let cmds = commands states
    let count = counter states
    put $ states { commands = cmd : cmds
                 , counter  = succ count
                 }
    return count

-- type
removeType :: KeySig -> Tx' ()
removeType keysig = do
    states <- get
    table <- gets typeTable
    put $ states { typeTable = Map.delete keysig table }

lookupType :: KeySig -> Tx' (Maybe Type)
lookupType keysig = do
    table <- gets typeTable
    return $ Map.lookup keysig table

insertType :: KeySig -> Type -> Tx' ()
insertType keysig typ = do
    states <- get
    table <- gets typeTable
    put $ states { typeTable = Map.insert keysig typ table }

-- type error
assertError :: TypeError -> Tx' a
assertError err = do
    states <- get
    let errors = typeError states
    let count = counter states
    put $ states { typeError = (count, err) : errors }
    return undefined


--------------------------------------------------------------------------------
--  send command
--------------------------------------------------------------------------------

sendCommand :: (Value a) => (Reply -> Either Reply a) -> Command -> Tx a
sendCommand decoder cmd = do
    count <- insertCommand cmd
    return $ Deferred (decoder . select count)
    where   select = flip (!!)


decodeAsAnything :: Value a => Reply -> Either Reply a
decodeAsAnything (Bulk (Just raw)) = case de raw of
    Left er -> Left (Error $ pack er)
    Right v -> Right v
decodeAsAnything others = error $ "should be (Bulk _), but got " ++ show others


decodeAsMaybe :: Value a => Reply -> Either Reply (Maybe a)
decodeAsMaybe (Bulk (Just raw)) = case de raw of
    Left er -> Left (Error $ pack er)
    Right v -> Right (Just v)
decodeAsMaybe (Bulk Nothing) = Right Nothing
decodeAsMaybe others = error $ "should be (Bulk _), but got " ++ show others

decodeAsList :: Value a => Reply -> Either Reply [a]
decodeAsList (MultiBulk (Just raw)) = mapM decodeAsAnything raw
decodeAsList others = error $ "should be (MultiBulk (Just _)), but got " ++ show others

decodeAsInt :: Reply -> Either Reply Int
decodeAsInt (Integer n) = Right (fromInteger n)
decodeAsInt others = error $ "should be (Integer _), but got " ++ show others

decodeAsStatus :: Reply -> Either Reply Status
decodeAsStatus (SingleLine "OK") = Right Ok
decodeAsStatus (SingleLine "PONG") = Right Pong
decodeAsStatus (SingleLine s) = Right (Status s)
decodeAsStatus others = error $ "should be (SingleLine _), but got " ++ show others

decodeAsBool :: Reply -> Either Reply Bool
decodeAsBool (Integer 1) = Right True
decodeAsBool (Integer 0) = Right False
decodeAsBool others = error $ "should be (Integer _), but got " ++ show others

returnAnything :: Value a => Command -> Tx a
returnAnything = sendCommand decodeAsAnything
returnMaybe :: Value a => Command -> Tx (Maybe a)
returnMaybe = sendCommand decodeAsMaybe
returnInt :: Command -> Tx Int
returnInt = sendCommand decodeAsInt
returnList :: Value a => Command -> Tx [a]
returnList = sendCommand decodeAsList
returnStatus :: Command -> Tx Status
returnStatus = sendCommand decodeAsStatus
returnBool :: Command -> Tx Bool
returnBool = sendCommand decodeAsBool

toRedisCommand :: Command -> Redis (Either Reply Redis.Status)
toRedisCommand PING = sendRequest ["PING"]
-- string
toRedisCommand (SET key val) = sendRequest ["SET", key, en val]
toRedisCommand (GET key) = sendRequest ["GET", key]
toRedisCommand (DEL key) = sendRequest ["DEL", key]
toRedisCommand (INCR key) = sendRequest ["INCR", key]
toRedisCommand (DECR key) = sendRequest ["DECR", key]
-- list
toRedisCommand (LPUSH key val) = sendRequest ["LPUSH", key, en val]
toRedisCommand (LPOP key) = sendRequest ["LPOP", key]
toRedisCommand (LLEN key) = sendRequest ["LLEN", key]
toRedisCommand (LRANGE key m n) = sendRequest ["LRANGE", key, en m, en n]
toRedisCommand (LINDEX key n) = sendRequest ["LINDEX", key, en n]
-- set
toRedisCommand (SADD key val) = sendRequest ["SADD", key, en val]
toRedisCommand (SREM key) = sendRequest ["SREM", key]
toRedisCommand (SCARD key) = sendRequest ["SCARD", key]
toRedisCommand (SMEMBERS key) = sendRequest ["SMEMBERS", key]
toRedisCommand (SPOP key) = sendRequest ["SPOP", key]
-- set
toRedisCommand (HSET key field val) = sendRequest ["HSET", key, field, en val]
-- toRedisCommand others = error $ "unrecognized command: " ++ show others
--------------------------------------------------------------------------------
--  Tx'
--------------------------------------------------------------------------------

-- execute
execTx :: Value a => Redis.Connection -> Tx a -> IO (Either Reply a)
execTx conn f = runRedis conn $ do

    -- issue MULTI
    _ <- multi

    -- extract states
    let (Deferred deferred, states) = runState f defaultTxState
    let redisCommands = reverse (commands states)

    -- run them all
    sequence_ (map toRedisCommand redisCommands)

    -- issue EXEC
    execResult <- exec
    liftIO $ print execResult
    case execResult of
        MultiBulk (Just replies) -> do
            return (deferred replies)
        _ -> error "something went wrong"

    where
        multi :: Redis (Either Reply Redis.Status)
        multi = sendRequest ["MULTI"]
        exec :: Redis Reply
        exec = either id id <$> sendRequest ["EXEC"]

checkTx :: Value a => Tx a -> [(Int, TypeError)]
checkTx f = reverse $ typeError (execState f defaultTxState)

-- check & exec
runTx :: Value a => Redis.Connection -> Tx a -> IO (Either [(Int, TypeError)] (Either Reply a))
runTx conn f = do
    -- see if there's any type error
    let errors = checkTx f
    if null errors
        then Right <$> execTx conn f    -- if none, then execute
        else Left  <$> return errors    -- else return the errors
