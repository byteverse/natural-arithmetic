{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}

module Arithmetic.Equal
  ( symmetric
  , plusR
  , plusL
  ) where

import Arithmetic.Unsafe (type (:=:)(Equal))
import GHC.TypeNats (type (+))

symmetric :: (m :=: n) -> (n :=: m)
symmetric Equal = Equal

plusL :: forall c m n. (m :=: n) -> (c + m :=: c + n)
plusL Equal = Equal

plusR :: forall c m n. (m :=: n) -> (m + c :=: n + c)
plusR Equal = Equal
