{-# LANGUAGE DeriveGeneric, DeriveDataTypeable
    , GADTs, RankNTypes
    , DataKinds, PolyKinds #-}

module Edis.Type where

import           Edis.Serialize

import           GHC.Generics (Generic)
import           Control.Applicative (Applicative(..))
import           Control.Monad.State (State)
import           Data.Map (Map)
import           Data.List (intercalate)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Serialize (Serialize)
import           Data.Typeable
import           Database.Redis (Reply(..), Redis)


--------------------------------------------------------------------------------
--  Type annotation for users
--------------------------------------------------------------------------------

data List n = List n
    deriving (Eq, Show, Generic, Typeable)

instance Serialize n => Serialize (List n)
instance Value n => Value (List n)

data SetD n = SetD n
    deriving (Eq, Show, Generic, Typeable)

instance Serialize n => Serialize (SetD n)
instance Value n => Value (SetD n)

data Hash = Hash
    deriving (Eq, Show, Generic, Typeable)

instance Serialize Hash
instance Value Hash

--------------------------------------------------------------------------------
--  Key & Field
--------------------------------------------------------------------------------

data KeySig = Sing Key | Pair Key Field
    deriving (Eq, Ord)

instance Show KeySig where
    show (Sing key) = unpack key
    show (Pair key field) = unpack key ++ " " ++ unpack field
--------------------------------------------------------------------------------
--  Type
--------------------------------------------------------------------------------

data Type = Anything
          | Type TypeRep
          | ListType  TypeRep
          | ListOfAnything
          | SetType  TypeRep
          | SetOfAnything
          | HashType

instance Show Type where
    show Anything = "Anything"
    show (Type n) = show n
    show (ListType  n) = "List " ++ show n
    show ListOfAnything = "List a"
    show (SetType  n) = "Set " ++ show n
    show SetOfAnything = "Set a"
    show HashType = "Hash"

instance Eq Type where
    Anything    == _                = True
    _           == Anything         = True
    Type s      == Type t           = s == t
    Type _      == _                = False
    ListType s  == ListType t       = s == t
    ListType _  == ListOfAnything   = True
    ListType _  == _                = False
    SetType s   == SetType t        = s == t
    SetType _   == SetOfAnything    = True
    SetType _   == _                = False
    HashType    == HashType         = True
    HashType    == _                = False
    ListOfAnything == ListType _    = True
    ListOfAnything == ListOfAnything = True
    ListOfAnything == _             = False
    SetOfAnything == SetType _      = True
    SetOfAnything == SetOfAnything  = True
    SetOfAnything == _              = False


--------------------------------------------------------------------------------
--  TypeError
--------------------------------------------------------------------------------

data TypeError
    = Undeclared KeySig
    | TypeMismatch KeySig Type Type

instance Show TypeError where
    show (Undeclared key) = "Undeclared: " ++ show key
    show (TypeMismatch _ expect got) = "TypeMismatch: expect '" ++ show expect ++ "', got '" ++ show got ++ "'"

--------------------------------------------------------------------------------
--  Deferred
--------------------------------------------------------------------------------

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
--  Tx
--------------------------------------------------------------------------------

type Tx' = State TxState
type Tx a = Tx' (Deferred a)
type Key = ByteString
type Field = ByteString

data TxState = TxState
    {   commands :: [Command]
    ,   typeTable :: Map KeySig Type
    ,   typeError :: [(Int, TypeError)]
    ,   counter :: Int
    }

--------------------------------------------------------------------------------
--  Status
--------------------------------------------------------------------------------

data Status = Pong | Ok | Status ByteString
    deriving (Generic, Typeable, Eq, Show)

instance Serialize Status
instance Value Status

--------------------------------------------------------------------------------
--  Command
--------------------------------------------------------------------------------

data Command where
    -- connection
    PING :: Command
    -- string
    SET :: Value a => Key -> a -> Command
    GET :: Key -> Command
    DEL :: Key -> Command
    INCR :: Key -> Command
    DECR :: Key -> Command

    -- list
    LPUSH :: Value a => Key -> a -> Command
    LPOP :: Key -> Command
    LLEN :: Key -> Command
    LRANGE :: Key -> Integer -> Integer -> Command
    LINDEX :: Key -> Integer -> Command

    -- set
    SADD :: Value a => Key -> a -> Command
    SREM :: Key -> Command
    SCARD :: Key -> Command
    SMEMBERS :: Key -> Command
    SPOP :: Key -> Command

    -- hash
    HSET :: Value a => Key -> Field -> a -> Command
    HDEL :: Key -> Field -> Command
    HKEYS :: Key -> Command
    HLEN :: Key -> Command

instance Show Command where
    show PING           = "PING"
    -- string
    show (SET k v)      = "SET " ++ unpack k ++ " " ++ unpack (en v)
    show (GET k)        = "GET " ++ unpack k
    show (DEL k)        = "DEL " ++ unpack k
    show (INCR k)       = "INCR " ++ unpack k
    show (DECR k)       = "DECR " ++ unpack k
    -- list
    show (LPUSH k v)    = "LPUSH " ++ unpack k ++ " " ++ unpack (en v)
    show (LPOP k)       = "LPOP " ++ unpack k
    show (LLEN k)       = "LLEN " ++ unpack k
    show (LRANGE k m n) = "LRANGE " ++ unpack k ++ " " ++ unpack (en m) ++ " " ++ unpack (en n)
    show (LINDEX k n)   = "LINDEX " ++ unpack k ++ " " ++ unpack (en n)
    -- set
    show (SADD k v)     = "SADD " ++ unpack k ++ " " ++ unpack (en v)
    show (SREM k)       = "SREM " ++ unpack k
    show (SCARD k)      = "SCARD " ++ unpack k
    show (SMEMBERS k)   = "SMEMBERS " ++ unpack k
    show (SPOP k)       = "SCARD " ++ unpack k
    -- hash
    show (HSET k f v)   = "HSET " ++ unpack k ++ " " ++ unpack f ++ " " ++ unpack (en v)
    show (HDEL k f)     = "HDEL " ++ unpack k ++ " " ++ unpack f
    show (HKEYS k)      = "HKEYS " ++ unpack k
    show (HLEN k)       = "HLEN " ++ unpack k

--------------------------------------------------------------------------------
--  P
--------------------------------------------------------------------------------

class PMonad m where
    unit :: a -> m p p a
    bind :: m p q a -> (a -> m q r b) -> m p r b

data P p q a = P { unP :: Redis a }

instance PMonad P where
    unit = P . return
    bind m f = P (unP m >>= unP . f )

--------------------------------------------------------------------------------
--  Kinds
--------------------------------------------------------------------------------

data ListK n = ListK n
data SetK n = SetK n
data HashK n = HashK n
