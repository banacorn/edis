module Tredis where

import Database.Redis as R hiding (decode)
import Data.Typeable
import Data.ByteString
import Data.Serialize as S
-- import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Map as Map
import Data.Map

type Key = ByteString
type TypeTable = Map Key TypeRep
type Tredis = StateT TypeTable Redis
data TredisReply = RedisReply Reply
                 | DecodeError String
                 | Undeclared Key
                 | TypeError Key TypeRep TypeRep
                 deriving (Show)

encode :: Serialize a => a -> ByteString
encode = S.encode

decode :: Serialize a => Either Reply (Maybe ByteString) -> Either TredisReply (Maybe a)
decode (Left err) = Left (RedisReply err)   -- error from Redis
decode (Right Nothing) = Right Nothing      -- nil
decode (Right (Just raw)) = case S.decode raw of
    Right val -> Right (Just val)           -- ok
    Left err  -> Left (DecodeError err)     -- decode error

typeOfInteger :: TypeRep
typeOfInteger = typeOf (0 :: Integer)

mapRedisReply :: Either Reply a -> Either TredisReply a
mapRedisReply (Left reply)  = Left $ RedisReply reply
mapRedisReply (Right state) = Right state

runTredis :: Connection -> Tredis a -> IO a
runTredis conn f = runRedis conn $ evalStateT f Map.empty

set :: (Serialize a, Typeable a) => ByteString -> a -> Tredis (Either TredisReply Status)
set key val = do
    modify $ insert key (typeOf val)
    lift $ mapRedisReply <$> R.set key (Tredis.encode val)

incr :: ByteString -> Tredis (Either TredisReply Integer)
incr key = do
    result <- gets (Map.lookup key)
    case result of
        Just ty -> case ty == typeOfInteger of
            True -> lift $ mapRedisReply <$> R.incr key
            False -> return $ Left $ TypeError key typeOfInteger ty
        Nothing -> return $ Left $ Undeclared key

get :: (Serialize a, Typeable a) => ByteString -> Tredis (Either TredisReply (Maybe a))
get key = lift $ Tredis.decode <$> R.get key
