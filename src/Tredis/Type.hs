{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

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
import           Database.Redis (Redis, Reply(..), Status(..))


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
    {   commands :: [Redis (Either Reply Status)]
    ,   typeTable :: Map Key Type
    ,   typeError :: [(Int, TypeError)]
    ,   counter :: Int
    }

data Type = Type TypeRep
          | List' TypeRep
          | ListOfAnything
          | Set' TypeRep
          | SetOfAnything
          deriving (Show)

instance Eq Type where
    Type s == Type t = s == t
    Type s == _ = False
    List' s == List' t = s == t
    List' s == ListOfAnything = True
    List' s == _ = False
    Set' s == Set' t = s == t
    Set' s == SetOfAnything = True
    Set' s == _ = False

data List n = List n
    deriving (Eq, Show, Generic, Typeable)

instance Serialize n => Serialize (List n)
instance Se n => Se (List n)
