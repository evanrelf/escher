{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Escher.Packets
  ( PacketWith (..)
  , Packet
  , HandshakeData (..)
  , Handshake
  , StatusRequest
  , StatusResponseData (..)
  , StatusResponse
  , statusResponse
  )
where

import Data.Aeson ((.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id, length)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Serialize as Cereal
import qualified Data.Text.Lazy as LText
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

data StatusResponseData = StatusResponseData
  { json :: Escher.String 0
  } deriving stock Generic
    deriving anyclass Cereal.Serialize

statusResponse :: StatusResponse
statusResponse = Packet
  { length = Escher.VarInt -1
  , id = Escher.VarInt 0x00
  , data_ = StatusResponseData
      { json
          = fromMaybe (error "unreachable")
          . Escher.mkString
          . LText.toStrict
          . Aeson.encodeToLazyText
          $ Aeson.object
              [ "version" .= Aeson.object
                  [ "name" .= ("1.16.5" :: Text)
                  , "protocol" .= (754 :: Int)
                  ]
              , "players" .= Aeson.object
                  [ "max" .= (10 :: Int)
                  , "online" .= (0 :: Int)
                  , "sample" .= ([] :: [()])
                  ]
              , "description" .= Aeson.object
                  [ "text" .= ("Hello world" :: Text)
                  ]
              , "favicon" .= ("data:image/png;base64," :: Text)
              ]
      }
  }

type StatusResponse = PacketWith StatusResponseData
