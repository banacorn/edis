{-# LANGUAGE DataKinds, PolyKinds
    , TypeFamilies, TypeOperators
    , GADTs
    , FlexibleInstances, FlexibleContexts #-} --, KindSignatures, ConstraintKinds #-}

module Edis.Dict where

import GHC.TypeLits
import Data.Proxy

-- fromJustEx0 :: (FromJust (Just Int) ~ Int) => ()
-- fromJustEx0 = ()
--
-- fromJustEx1 :: (FromJust Nothing ~ Int) => ()
-- fromJustEx1 = ()
