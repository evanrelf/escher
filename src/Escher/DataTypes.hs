{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import qualified Data.Serialize as Cereal
import qualified Data.Serialize.LEB128 as Leb128
import qualified Data.Text.Encoding as Text
import qualified Prelude

data TODO

newtype Boolean = Boolean Bool

newtype Byte = Byte Int8

newtype UnsignedByte = UnsignedByte Word8

newtype Short = Short Int16

newtype UnsignedShort = UnsignedShort Word16
  deriving stock Show
  deriving newtype Cereal.Serialize

newtype Int = Int Int32

newtype Long = Long Int64

newtype Float = Float Prelude.Float

newtype Double = Double Prelude.Double

data String (length :: Nat) = String
  { size :: VarInt
  , string :: Text
  } deriving stock Show

instance Cereal.Serialize (String n) where
  put :: Cereal.Putter (String n)
  put String{size, string} = do
    Cereal.put size
    Cereal.putByteString (Text.encodeUtf8 string)

  get :: Cereal.Get (String n)
  get = do
    size <- Cereal.get
    string <- do
      bytes <- Cereal.getByteString (fromIntegral $ unVarInt size)
      pure (Text.decodeUtf8 bytes)
    pure String{size, string}

newtype Chat = Chat (String 262144)

newtype Identifier = Identifier (String 32767)

newtype VarInt = VarInt { unVarInt :: Int32 }
  deriving stock Show

-- TODO: Enforce 1-5 bytes
instance Cereal.Serialize VarInt where
  put :: Cereal.Putter VarInt
  put = Leb128.putSLEB128 . unVarInt

  get :: Cereal.Get VarInt
  get = VarInt <$> Leb128.getSLEB128

newtype VarLong = VarLong { unVarLong :: Int64 }

-- TODO: Enforce 1-10 bytes
instance Cereal.Serialize VarLong where
  put :: Cereal.Putter VarLong
  put = Leb128.putSLEB128 . unVarLong

  get :: Cereal.Get VarLong
  get = VarLong <$> Leb128.getSLEB128

newtype EntityMetadata = EntityMetadata TODO

newtype Slot = Slot TODO

newtype NbtTag = NbtTag TODO

newtype Position = Position TODO

newtype Angle = Angle Word8

newtype Uuid = Uuid TODO

newtype Optional a = Optional (Maybe a)

newtype Array a = Array [a]

newtype Enum a = Enum a
  deriving stock Show
  deriving newtype Cereal.Serialize

newtype ByteArray = ByteArray LByteString.ByteString
  deriving stock Show
