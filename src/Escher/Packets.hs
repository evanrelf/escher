{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Escher.Packets
  ( PacketWith (..)
  , Packet
  , HandshakeData (..)
  , Handshake
  , StatusRequest
  )
where

import GHC.Generics (Generic)
import Prelude hiding (id, length)

import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Serialize as Cereal
import qualified Escher.Types as Escher

data PacketWith data_ = Packet
  { length :: Escher.VarInt
  , id :: Escher.VarInt
  , data_ :: data_
  } deriving stock (Generic, Show)
    deriving anyclass Cereal.Serialize

type Packet = PacketWith Escher.ByteArray

instance {-# OVERLAPPING #-} Cereal.Serialize Packet where
  put :: Cereal.Putter Packet
  put Packet{length, id, data_ = Escher.ByteArray bytes} = do
    Cereal.put length
    Cereal.put id
    Cereal.putLazyByteString bytes

  get :: Cereal.Get Packet
  get = do
    length <- Cereal.get
    beforeId <- Cereal.bytesRead
    id <- Cereal.get
    afterId <- Cereal.bytesRead
    let idLength = beforeId - afterId
    let dataLength = fromIntegral (Escher.unVarInt length) - idLength
    data_ <- do
      bytes <- LByteString.fromStrict <$> Cereal.getBytes dataLength
      pure (Escher.ByteArray bytes)
    pure Packet{length, id, data_}

data HandshakeData = HandshakeData
  { protocolVersion :: Escher.VarInt
  , serverAddress :: Escher.String 255
  , serverPort :: Escher.UnsignedShort
  , nextState :: Escher.Enum Escher.VarInt
  } deriving stock (Generic, Show)
    deriving anyclass Cereal.Serialize

type Handshake = PacketWith HandshakeData

type StatusRequest = PacketWith ()
