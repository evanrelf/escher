{-# LANGUAGE DataKinds #-}

module Escher.Packets
  ( Packet (..)
  , Handshake (..)
  )
where

import qualified Escher.DataTypes as Escher

data Packet = Packet
  { length :: Escher.VarInt
  , packetId :: Escher.VarInt
  , data_ :: Escher.ByteArray
  }

data Handshake = Handshake
  { protocolVersion :: Escher.VarInt
  , serverAddress :: Escher.String 255
  , serverPort :: Escher.UnsignedShort
  , nextState :: Escher.VarInt
  }
