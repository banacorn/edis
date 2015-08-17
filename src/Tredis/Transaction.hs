{-# LANGUAGE OverloadedStrings, GADTs, DeriveDataTypeable, TypeOperators #-}

module Tredis.Transaction where

import           Data.Typeable
import           Control.Applicative
import           Control.Monad.State
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import qualified Data.Serialize as S
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

data Deferred a = Deferred ([Reply] -> Either String a)
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

class Serialize a => Se a where
    en :: a -> ByteString
    en = S.encode
    de :: ByteString -> Either String a
    de = S.decode

instance Se Int where
    en = pack . show
    de = Right . read . unpack

instance Se ()

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

sendCommand :: (Se a, Typeable a) => [ByteString] -> Tx (Deferred a)
sendCommand = sendCommand' decodeReply

sendCommand' :: (Se a, Typeable a) => (Reply -> Either String a) -> [ByteString] -> Tx (Deferred a)
sendCommand' decoder cmd = do
    count <- insertCommand cmd
    return $ Deferred (decoder . select count)
    where   select = flip (!!)

decodeReply :: (Se a, Typeable a) => Reply -> Either String a
decodeReply (Bulk (Just raw)) =
    case de raw of
        Left  err -> Left err
        Right val -> if typeOf val == typeRep (Proxy :: Proxy Int)
                        then Right val
                        else Right val
decodeReply others = Left (show others)




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

-- type stuffs
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

-- checkType

deferredValueType :: Typeable a => Deferred a -> TypeRep
deferredValueType q = head (typeRepArgs (typeOf q))

buildListType :: TypeRep -> TypeRep
buildListType arg = mkTyConApp (typeRepTyCon $ typeOf [()]) [arg]

--------------------------------------------------------------------------------
--  Tx
--------------------------------------------------------------------------------

-- execute
execTx :: Se a => Tx (Deferred a) -> Redis (Either String a)
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
    liftIO $ print execResult
    case execResult of
        MultiBulk (Just replies) -> do
            return (deferred replies)
        _ -> error "something went wrong"

    where
        multi :: Redis (Either Reply Status)
        multi = sendRequest ["MULTI"]
        exec :: Redis Reply
        exec = either id id <$> sendRequest ["EXEC"]

runTx :: Se a => Redis.Connection -> Tx (Deferred a) -> IO (Either [(Int, TypeError)] (Either String a))
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
