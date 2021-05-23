{-# LANGUAGE DataKinds #-}

module Escher.Packets
  ( PacketWith (..)
  , Packet
  , HandshakeData (..)
  , Handshake
  )
where

import qualified Escher.DataTypes as Escher

data PacketWith data_ = Packet
  { length :: Escher.VarInt
  , id :: Escher.VarInt
  , data_ :: data_
  }

type Packet = PacketWith Escher.ByteArray

data HandshakeData = HandshakeData
  { protocolVersion :: Escher.VarInt
  , serverAddress :: Escher.String 255
  , serverPort :: Escher.UnsignedShort
  , nextState :: Escher.Enum Escher.VarInt
  }

type Handshake = PacketWith HandshakeData
