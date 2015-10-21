-- {-# LANGUAGE  #-}
-- , MultiParamTypeClasses
--     , FunctionalDependencies, GADTs, UndecidableInstances
--     , TypeFamilies
--  #-}

module Overlapping where

class ShowList a where
    showl :: [a] -> String
--
-- -- General instance
instance ShowList a where
    showl = const "general"

-- Special instances, for lists of specific types
instance ShowList Bool where showl = const "Bool"
-- instance ShowList Char where showl = id
