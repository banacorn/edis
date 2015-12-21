{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators, OverloadedStrings #-}

module Database.Edis.Command.ZSet where

import Database.Edis.Type
import Database.Edis.Helper

import Data.Proxy               (Proxy)
import Data.Serialize           (Serialize, encode)
import Data.Type.Bool
import Database.Redis as Redis hiding (decode)
import GHC.TypeLits

--------------------------------------------------------------------------------
--  Sorted Sets
--------------------------------------------------------------------------------

zadd :: (KnownSymbol s, Serialize x, ZSetOrNX xs s)
        => Proxy s -> Double -> x
        -> Edis xs (If (Member xs s) xs (Set xs s (ZSetOf x))) (Either Reply Integer)
zadd key score val = Edis $ Redis.zadd (encodeKey key) [(score, encode val)]


zcard :: (KnownSymbol s, SetOrNX xs s)
        => Proxy s
        -> Edis xs xs (Either Reply Integer)
zcard key = Edis $ Redis.zcard (encodeKey key)

zcount :: (KnownSymbol s, SetOrNX xs s)
        => Proxy s -> Double -> Double
        -> Edis xs xs (Either Reply Integer)
zcount key min' max' = Edis $ Redis.zcount (encodeKey key) min' max'

zincrby :: (KnownSymbol s, Serialize x, ZSetOrNX xs s)
        => Proxy s -> Integer -> x
        -> Edis xs (If (Member xs s) xs (Set xs s (ZSetOf x))) (Either Reply Double)
zincrby key score val = Edis $ Redis.zincrby (encodeKey key) score (encode val)

zrange :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Integer -> Integer
        -> Edis xs xs (Either Reply [x])
zrange key from to = Edis $ Redis.zrange (encodeKey key) from to >>= decodeAsList

zrangeWithscores :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Integer -> Integer
        -> Edis xs xs (Either Reply [(x, Double)])
zrangeWithscores key from to =
    Edis $ Redis.zrangeWithscores (encodeKey key) from to >>= decodeAsListWithScores

zrangebyscore :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double
        -> Edis xs xs (Either Reply [x])
zrangebyscore key min' max' =
    Edis $ Redis.zrangebyscore (encodeKey key) min' max' >>= decodeAsList

zrangebyscoreWithscores :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double
        -> Edis xs xs (Either Reply [(x, Double)])
zrangebyscoreWithscores key min' max' =
    Edis $ Redis.zrangebyscoreWithscores (encodeKey key) min' max' >>= decodeAsListWithScores

zrangebyscoreLimit :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double -> Integer -> Integer
        -> Edis xs xs (Either Reply [x])
zrangebyscoreLimit key min' max' offset count =
    Edis $ Redis.zrangebyscoreLimit (encodeKey key) min' max' offset count >>= decodeAsList

zrangebyscoreWithscoresLimit :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double -> Integer -> Integer
        -> Edis xs xs (Either Reply [(x, Double)])
zrangebyscoreWithscoresLimit key min' max' offset count =
    Edis $ Redis.zrangebyscoreWithscoresLimit (encodeKey key) min' max' offset count >>= decodeAsListWithScores

zrank :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> x
        -> Edis xs xs (Either Reply (Maybe Integer))
zrank key val = Edis $ Redis.zrank (encodeKey key) (encode val)

zremrangebyrank :: (KnownSymbol s, ZSetOrNX xs s)
        => Proxy s -> Integer -> Integer
        -> Edis xs xs (Either Reply Integer)
zremrangebyrank key from to = Edis $ Redis.zremrangebyrank (encodeKey key) from to

zremrangebyscore :: (KnownSymbol s, ZSetOrNX xs s)
        => Proxy s -> Double -> Double
        -> Edis xs xs (Either Reply Integer)
zremrangebyscore key min' max' = Edis $ Redis.zremrangebyscore (encodeKey key) min' max'

zrevrange :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Integer -> Integer
        -> Edis xs xs (Either Reply [x])
zrevrange key from to = Edis $ Redis.zrevrange (encodeKey key) from to >>= decodeAsList

zrevrangeWithscores :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Integer -> Integer
        -> Edis xs xs (Either Reply [(x, Double)])
zrevrangeWithscores key from to =
    Edis $ Redis.zrevrangeWithscores (encodeKey key) from to >>= decodeAsListWithScores

zrevrangebyscore :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double
        -> Edis xs xs (Either Reply [x])
zrevrangebyscore key min' max' =
    Edis $ Redis.zrevrangebyscore (encodeKey key) min' max' >>= decodeAsList

zrevrangebyscoreWithscores :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double
        -> Edis xs xs (Either Reply [(x, Double)])
zrevrangebyscoreWithscores key min' max' =
    Edis $ Redis.zrevrangebyscoreWithscores (encodeKey key) min' max' >>= decodeAsListWithScores

zrevrangebyscoreLimit :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double -> Integer -> Integer
        -> Edis xs xs (Either Reply [x])
zrevrangebyscoreLimit key min' max' offset count =
    Edis $ Redis.zrevrangebyscoreLimit (encodeKey key) min' max' offset count >>= decodeAsList

zrevrangebyscoreWithscoresLimit :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> Double -> Double -> Integer -> Integer
        -> Edis xs xs (Either Reply [(x, Double)])
zrevrangebyscoreWithscoresLimit key min' max' offset count =
    Edis $ Redis.zrevrangebyscoreWithscoresLimit (encodeKey key) min' max' offset count >>= decodeAsListWithScores

zrevrank :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> x
        -> Edis xs xs (Either Reply (Maybe Integer))
zrevrank key val = Edis $ Redis.zrevrank (encodeKey key) (encode val)

zscore :: (KnownSymbol s, Serialize x
        , 'Just (ZSetOf x) ~ Get xs s)
        => Proxy s -> x
        -> Edis xs xs (Either Reply (Maybe Double))
zscore key val = Edis $ Redis.zscore (encodeKey key) (encode val)
