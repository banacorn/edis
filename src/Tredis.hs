module Tredis where

import Database.Redis as R hiding (decode, put)
import Data.Typeable
import Data.ByteString
import Data.Serialize as S hiding (get, put)
import Control.Monad.State as State
import Control.Applicative ((<$>))
import Data.Map as Map
import Data.Map

--------------------------------------------------------------------------------
--  encoding & decoding
--------------------------------------------------------------------------------

encodeValue :: Serialize a => a -> ByteString
encodeValue = S.encode


decodeValue :: Serialize a => ByteString -> Tredis (Maybe a)
decodeValue raw = case S.decode raw of
    Right val -> return $ Just val
    Left err -> do
        assertError $ DecodeError err
        return Nothing

-- decodeValue Nothing    = Nothing
-- decodeValue (Just raw) = case S.decode raw of
--     Right val -> Just val            -- ok
--     Left err  -> Nothing             -- decode error

-- decodeValue (Left err) = Left (RedisReply err)   -- error from Redis
-- decodeValue (Right Nothing) = Right Nothing      -- nil
-- decodeValue (Right (Just raw)) = case S.decode raw of
--     Right val -> Right (Just val)           -- ok
--     Left err  -> Left (DecodeError err)     -- decode error

--------------------------------------------------------------------------------
--  Tredis
--------------------------------------------------------------------------------

type RedisReply a = Either Reply a

type Key = ByteString
-- Hash table for storing key-type relation
type TypeTable = Map Key TypeRep
data TredisState = TredisState
    {   typeTable :: TypeTable
    ,   typeError :: [TypeError]
    }

defaultTredisState :: TredisState
defaultTredisState = TredisState Map.empty []

data TypeError = Undeclared
               | TypeMismatch
               | DecodeError String
               deriving (Show)

type Tredis = StateT TredisState Redis
-- data TredisReply = RedisReply Reply     -- original Redis error reply
--                  | DecodeError String   -- decoding error
--                  | Undeclared Key       -- unable to determine the type of a key
--                  | TypeError Key TypeRep TypeRep
--                  deriving (Show)

-- assert key-type relation
assertType :: Key -> TypeRep -> Tredis ()
assertType key typ = do
    state <- State.get
    let table = typeTable state
    State.put (state { typeTable = Map.insert key typ table })

lookupType :: Key -> Tredis (Maybe TypeRep)
lookupType key = do
    state <- State.get
    let table = typeTable state
    return $ Map.lookup key table

assertError :: TypeError -> Tredis ()
assertError e = do
    state <- State.get
    let errors = typeError state
    State.put (state { typeError = e : errors })

liftDecode :: (Serialize a) => Redis (RedisReply (Maybe ByteString)) -> Tredis (RedisReply (Maybe a))
liftDecode f = lift f >>= g
    where   g (Left a)           = Left  <$> return a
            g (Right Nothing)    = Right <$> return Nothing
            g (Right (Just raw)) = Right <$> decodeValue raw

runTredis :: Connection -> Tredis a -> IO (Either [TypeError] a)
runTredis conn f = runRedis conn $ do
    (value, state) <- runStateT f defaultTredisState
    let typeErrors = typeError state
    if Prelude.null typeErrors
        then return (Right value)
        else return (Left typeErrors)

--------------------------------------------------------------------------------
--  commands
--------------------------------------------------------------------------------

set :: (Serialize a, Typeable a) => ByteString -> a -> Tredis (RedisReply Status)
set key val = do
    assertType key (typeOf val)
    lift $ R.set key (encodeValue val)

get :: (Serialize a, Typeable a) => ByteString -> Tredis (RedisReply (Maybe a))
get key = liftDecode $ R.get key

incr :: ByteString -> Tredis (RedisReply Integer)
incr key = do
    -- result <- gets (Map.lookup key)
    result <- lookupType key
    case result of
        Just ty -> case ty == typeOfInteger of
            True -> lift $ R.incr key                        -- Integer
            False -> do
                assertError TypeMismatch
                -- Right <$> Nothing
                return $ Right 0
                -- Left $ TypeError key typeOfInteger ty -- not Integer
        Nothing -> do -- key-type relation not found
            assertError Undeclared
            return $ Right 0
    where
            typeOfInteger :: TypeRep
            typeOfInteger = typeOf (0 :: Integer)
