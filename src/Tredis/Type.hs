{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GADTs, RankNTypes #-}

module Tredis.Type where

import           Tredis.Serialize

import           GHC.Generics
import           Control.Applicative (Applicative(..))
import           Control.Monad.State (State)
import           Data.Map (Map)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Serialize (Serialize)
import           Data.Typeable
import qualified Database.Redis as Redis
import           Database.Redis (Redis, Reply(..))


type Tx' = State TxState
type Tx a = Tx' (Deferred a)
type Key = ByteString

data TypeError
    = Undeclared Key
    | TypeMismatch Key Type Type

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

data TxState = TxState
    {   commands :: [Redis (Either Reply Redis.Status)]
    ,   typeTable :: Map Key Type
    ,   typeError :: [(Int, TypeError)]
    ,   counter :: Int
    }

data Type = Type TypeRep
          | ListType  TypeRep
          | ListOfAnything
          | SetType  TypeRep
          | SetOfAnything

instance Show Type where
    show (Type n) = show n
    show (ListType  n) = "List " ++ show n
    show ListOfAnything = "List a"
    show (SetType  n) = "Set " ++ show n
    show SetOfAnything = "Set a"

instance Eq Type where
    Type s == Type t = s == t
    Type s == _ = False
    ListType  s == ListType  t = s == t
    ListType  s == ListOfAnything = True
    ListType  s == _ = False
    SetType  s == SetType  t = s == t
    SetType  s == SetOfAnything = True
    SetType  s == _ = False

data RList n = RList n
    deriving (Eq, Show, Generic, Typeable)

instance Serialize n => Serialize (RList n)
instance Value n => Value (RList n)

data RSet n = RSet n
    deriving (Eq, Show, Generic, Typeable)

instance Serialize n => Serialize (RSet n)
instance Value n => Value (RSet n)

data Status = Pong | Ok | Status ByteString
    deriving (Generic, Typeable, Eq, Show)

instance Serialize Status
instance Value Status

-- data Command where
--     Set :: Key -> a -> Command
--     Del :: Key -> Command
--     Incr :: Key -> Command
