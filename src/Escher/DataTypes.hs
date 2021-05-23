{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Escher.DataTypes
  ( Boolean (..)
  , Byte (..)
  , UnsignedByte (..)
  , Short (..)
  , UnsignedShort (..)
  , Int (..)
  , Long (..)
  , Float (..)
  , Double (..)
  , String (..)
  , Chat (..)
  , Identifier (..)
  , VarInt (..)
  , VarLong (..)
  , EntityMetadata (..)
  , Slot (..)
  , NbtTag (..)
  , Position (..)
  , Angle (..)
  , Uuid (..)
  , Optional (..)
  , Array (..)
  , Enum (..)
  , ByteArray (..)
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Word (Word16, Word8)
import GHC.TypeLits (Nat)
import Prelude hiding (Double, Enum, Float, Int, String)

import qualified Data.ByteString.Lazy as LByteString
import qualified Prelude

data TODO

newtype Boolean = Boolean Bool

newtype Byte = Byte Int8

newtype UnsignedByte = UnsignedByte Word8

newtype Short = Short Int16

newtype UnsignedShort = UnsignedShort Word16

newtype Int = Int Int32

newtype Long = Long Int64

newtype Float = Float Prelude.Float

newtype Double = Double Prelude.Double

data String (length :: Nat) = String
  { size :: VarInt
  , string :: Text
  }

newtype Chat = Chat (String 262144)

newtype Identifier = Identifier (String 32767)

newtype VarInt = VarInt Int32

newtype VarLong = VarLong Int64

newtype EntityMetadata = EntityMetadata TODO

newtype Slot = Slot TODO

newtype NbtTag = NbtTag TODO

newtype Position = Position TODO

newtype Angle = Angle Word8

newtype Uuid = Uuid TODO

newtype Optional a = Optional (Maybe a)

newtype Array a = Array [a]

newtype Enum a = Enum a

newtype ByteArray = ByteArray LByteString.ByteString
