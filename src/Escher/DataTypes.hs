{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Escher.DataTypes
  ( UnsignedShort
  , String (..)
  , VarInt
  , ByteArray
  )
where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word16)
import GHC.TypeLits (Nat)

import qualified Data.ByteString.Lazy as LByteString

type UnsignedShort = Word16

data String (length :: Nat) = String
  { size :: VarInt
  , string :: Text
  }

type VarInt = Int32

type ByteArray = LByteString.ByteString
