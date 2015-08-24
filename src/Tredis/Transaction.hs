{-# LANGUAGE OverloadedStrings #-}

module Tredis.Transaction where

import           Tredis.Serialize
import           Tredis.Type

import           Data.Typeable
import           Control.Applicative ((<$>))
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize)
import qualified Data.Map as Map
import qualified Database.Redis as Redis
import           Database.Redis (Redis, runRedis, Reply(..), sendRequest)

--------------------------------------------------------------------------------
--  TxState manipulation
--------------------------------------------------------------------------------

defaultTxState :: TxState
defaultTxState = TxState [] Map.empty [] 0

-- commands
insertCommand :: [ByteString] -> Tx' Int
insertCommand cmd = do
    state <- get
    let cmds = commands state
    let count = counter state
    put $ state { commands = sendRequest cmd : cmds
                , counter  = succ count
                }
    return count

-- type
removeType :: Key -> Tx' ()
removeType key = do
    state <- get
    let table = typeTable state
    put $ state { typeTable = Map.delete key table }

lookupType :: Key -> Tx' (Maybe Type)
lookupType key = do
    state <- get
    let table = typeTable state
    return $ Map.lookup key table

insertType :: Key -> Type -> Tx' ()
insertType key typ = do
    state <- get
    let table = typeTable state
    put $ state { typeTable = Map.insert key typ table }

-- type error
assertError :: TypeError -> Tx' a
assertError err = do
    state <- get
    let errors = typeError state
    let count = counter state
    put $ state { typeError = (count, err) : errors }
    return undefined


--------------------------------------------------------------------------------
--  send command
--------------------------------------------------------------------------------

toStatus :: Redis.Status -> Status
toStatus Redis.Pong = Pong
toStatus Redis.Ok = Ok
toStatus (Redis.Status n) = Status n

sendCommand :: (Value a) => (Reply -> Either Reply a) -> [ByteString] -> Tx a
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

returnAnything :: Value a => [ByteString] -> Tx a
returnAnything = sendCommand decodeAsAnything
returnMaybe :: Value a => [ByteString] -> Tx (Maybe a)
returnMaybe = sendCommand decodeAsMaybe
returnInt :: [ByteString] -> Tx Int
returnInt = sendCommand decodeAsInt
returnList :: Value a => [ByteString] -> Tx [a]
returnList = sendCommand decodeAsList
returnStatus :: [ByteString] -> Tx Status
returnStatus = sendCommand decodeAsStatus


--------------------------------------------------------------------------------
--  Tx'
--------------------------------------------------------------------------------

-- execute
execTx :: Value a => Tx a -> Redis (Either Reply a)
execTx f = do

    -- issue MULTI
    multi

    -- extract states
    let (Deferred deferred, state) = runState f defaultTxState
    let redisCommands = reverse $ commands state

    -- run them all
    sequence redisCommands

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
checkTx f =
    let state = execState f defaultTxState in
    reverse $ typeError state

runTx :: (Serialize a, Value a) => Redis.Connection -> Tx a -> IO (Either [(Int, TypeError)] (Either Reply a))
runTx conn f = runRedis conn $ do
    let state = execState f defaultTxState

    -- see if there's any type error
    let errors = typeError state
    if null errors
        then Right <$> execTx f         -- if none, then execute
        else Left  <$> return (reverse errors)    -- else return the errors
