{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Tredis.Transaction where

import Tredis.Serialize

import           Data.Typeable
import           Control.Applicative
import           Control.Monad.State
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize)
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

data Deferred a = Deferred ([Reply] -> Either Reply a)
    deriving Typeable

instance Functor Deferred where
    fmap f (Deferred g) = Deferred (fmap f . g)

instance Applicative Deferred where
    pure x                = Deferred (const $ Right x)
    Deferred f <*> Deferred x = Deferred $ \rs -> do
                                        f' <- f rs
                                        x' <- x rs
                                        return (f' x')

instance Monad Deferred where
    return         = pure
    Deferred x >>= f = Deferred $ \rs -> do
                                x' <- x rs
                                let Deferred f' = f x'
                                f' rs

--------------------------------------------------------------------------------
--  TxState manipulation
--------------------------------------------------------------------------------

data TxState = TxState
    {   commands :: [Redis (Either Reply Status)]
    ,   typeTable :: Map Key TypeRep
    ,   typeError :: [(Int, TypeError)]
    ,   counter :: Int
    }

defaultTxState :: TxState
defaultTxState = TxState [] Map.empty [] 0

-- commands
insertCommand :: [ByteString] -> Tx Int
insertCommand cmd = do
    state <- get
    let cmds = commands state
    let count = counter state
    put $ state { commands = sendRequest cmd : cmds
                , counter  = succ count
                }
    return count

-- type
removeType :: Key -> Tx ()
removeType key = do
    state <- get
    let table = typeTable state
    put $ state { typeTable = Map.delete key table }

lookupType :: Key -> Tx (Maybe TypeRep)
lookupType key = do
    state <- get
    let table = typeTable state
    return $ Map.lookup key table

insertType :: Key -> TypeRep -> Tx ()
insertType key typeRep = do
    state <- get
    let table = typeTable state
    put $ state { typeTable = Map.insert key typeRep table }

-- type error
assertError :: TypeError -> Tx ()
assertError err = do
    state <- get
    let errors = typeError state
    let count = counter state
    put $ state { typeError = (count, err) : errors }




--------------------------------------------------------------------------------
--  send command
--------------------------------------------------------------------------------

sendCommand :: (Se a, Typeable a) => [ByteString] -> Tx (Deferred a)
sendCommand = sendCommand' decode

sendCommand' :: (Se a, Typeable a) => (Reply -> Either Reply a) -> [ByteString] -> Tx (Deferred a)
sendCommand' decoder cmd = do
    count <- insertCommand cmd
    return $ Deferred (decoder . select count)
    where   select = flip (!!)

decode :: (Se a, Typeable a) => Reply -> Either Reply a
decode (Bulk (Just raw)) = case de raw of
    Left er -> Left (Error $ pack er)
    Right v -> Right v
decode others = Left others

decodeAsInt :: Reply -> Either Reply Int
decodeAsInt (Integer n) = Right (fromInteger n)
decodeAsInt others = Left others

--------------------------------------------------------------------------------
--  type checking stuffs
--------------------------------------------------------------------------------

declareType :: Key -> TypeRep -> Tx ()
declareType key typeRep = do
    typeError <- checkType key typeRep
    case typeError of
        Nothing                   -> insertType key typeRep -- already asserted
        Just (Undeclared _)       -> insertType key typeRep -- not asserted yet
        Just (TypeMismatch k x y) -> assertError $ TypeMismatch k x y

declareTypeOfVal :: Typeable a => Key -> a -> Tx ()
declareTypeOfVal key val = declareType key (typeOf val)

(=::) :: Typeable a => Key -> a -> Tx ()
(=::) = declareTypeOfVal

checkType :: Key -> TypeRep -> Tx (Maybe TypeError)
checkType key got = do
    result <- lookupType key
    case result of
        Nothing -> do
            return $ Just (Undeclared key)
        Just expected -> if expected == got
            then return $ Nothing
            else return $ Just (TypeMismatch key expected got)

deferredValueType :: Typeable a => Deferred a -> TypeRep
deferredValueType q = head (typeRepArgs (typeOf q))

buildListType :: TypeRep -> TypeRep
buildListType arg = mkTyConApp (typeRepTyCon $ typeOf [()]) [arg]

--------------------------------------------------------------------------------
--  Tx
--------------------------------------------------------------------------------

-- execute
execTx :: Se a => Tx (Deferred a) -> Redis (Either Reply a)
execTx f = do

    -- issue MULTI
    multi

    -- extract states
    let (Deferred deferred, state) = runState f defaultTxState
    let (redisCommands) = reverse $ commands state

    -- run them all
    sequence redisCommands

    -- issue EXEC
    execResult <- exec
    -- liftIO $ print execResult
    case execResult of
        MultiBulk (Just replies) -> do
            return (deferred replies)
        _ -> error "something went wrong"

    where
        multi :: Redis (Either Reply Status)
        multi = sendRequest ["MULTI"]
        exec :: Redis Reply
        exec = either id id <$> sendRequest ["EXEC"]

runTx :: (Serialize a, Se a) => Redis.Connection -> Tx (Deferred a) -> IO (Either [(Int, TypeError)] (Either Reply a))
runTx conn f = runRedis conn $ do
    let state = execState f defaultTxState

    -- see if there's any type error
    let errors = typeError state
    if null errors
        then Right <$> execTx f         -- if none, then execute
        else Left  <$> return (reverse errors)    -- else return the errors

-- re-export Redis shit
connect = Redis.connect
defaultConnectInfo = Redis.defaultConnectInfo
