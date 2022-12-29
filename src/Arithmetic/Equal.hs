{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language TypeOperators #-}

module Arithmetic.Equal
  ( symmetric
  , plusR
  , plusL
  ) where

import Arithmetic.Unsafe (type (:=:)(Eq))
import GHC.TypeNats (type (+))

symmetric :: (m :=: n) -> (n :=: m)
{-# inline symmetric #-}
symmetric Eq = Eq

plusL :: forall c m n. (m :=: n) -> (c + m :=: c + n)
{-# inline plusL #-}
plusL Eq = Eq

plusR :: forall c m n. (m :=: n) -> (m + c :=: n + c)
{-# inline plusR #-}
plusR Eq = Eq
