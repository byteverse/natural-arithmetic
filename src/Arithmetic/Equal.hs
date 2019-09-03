{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ExplicitForAll #-}

module Arithmetic.Equal
  ( symmetric
  , plusR
  , plusL
  ) where

import Arithmetic.Unsafe (type (:=:)(Eq))
import GHC.TypeNats (type (+))

symmetric :: (m :=: n) -> (n :=: m)
symmetric Eq = Eq

plusL :: forall c m n. (m :=: n) -> (c + m :=: c + n)
plusL Eq = Eq

plusR :: forall c m n. (m :=: n) -> (m + c :=: n + c)
plusR Eq = Eq
