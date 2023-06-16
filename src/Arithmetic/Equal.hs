{-# language DataKinds #-}
{-# language ExplicitForAll #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}

module Arithmetic.Equal
  ( symmetric
  , plusR
  , plusL
  , plusR#
  , plusL#
  ) where

import Arithmetic.Unsafe (type (:=:)(Eq), type (:=:#)(Eq#))
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

plusL# :: forall c m n. (m :=:# n) -> (c + m :=:# c + n)
{-# inline plusL# #-}
plusL# _ = Eq# (# #)

plusR# :: forall c m n. (m :=:# n) -> (m + c :=:# n + c)
{-# inline plusR# #-}
plusR# _ = Eq# (# #)

