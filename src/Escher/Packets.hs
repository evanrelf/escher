{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Escher.Packets
  ( PacketWith (.., Packet)
  , Packet
  , HandshakeData (..)
  , Handshake
  , StatusRequest
  , StatusResponseData (..)
  , StatusResponse
  , statusResponse
  , Ping
  , Pong
  , pong
  )
where

import Data.Aeson ((.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id, length)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Serialize as Cereal
import qualified Data.Text.Lazy as LText
import qualified Escher.Types as Escher

data PacketWith data_ = UnsafePacket
  { length :: Escher.VarInt
  , id :: Escher.VarInt
  , data_ :: data_
  } deriving stock (Generic, Show)
    deriving anyclass Cereal.Serialize

type Packet = PacketWith Escher.ByteArray

pattern Packet
  :: Cereal.Serialize data_
  => Escher.VarInt
  -> data_
  -> PacketWith data_
pattern Packet id data_ <- UnsafePacket{id, data_} where
  Packet id data_ = UnsafePacket
    { length
        = Escher.VarInt
        . fromIntegral
        . ByteString.length
        $ Cereal.encode id <> Cereal.encode data_
    , id
    , data_
    }

instance {-# OVERLAPPING #-} Cereal.Serialize Packet where
  put :: Cereal.Putter Packet
  put UnsafePacket{length, id, data_ = Escher.ByteArray bytes} = do
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
    pure UnsafePacket{length, id, data_}

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
  } deriving stock (Generic, Show)
    deriving anyclass Cereal.Serialize

type StatusResponse = PacketWith StatusResponseData

statusResponse :: StatusResponse
statusResponse =
  Packet 0x00 StatusResponseData
    { json
        = Escher.String
        . LText.toStrict
        . Aeson.encodeToLazyText
        $ Aeson.object
            [ "version" .= Aeson.object
                [ "name" .= ("1.17.1" :: Text)
                , "protocol" .= (756 :: Int)
                ]
            , "players" .= Aeson.object
                [ "max" .= (10 :: Int)
                , "online" .= (0 :: Int)
                , "sample" .= ([] :: [()])
                ]
            , "description" .= Aeson.object
                [ "text" .= ("Hello world" :: Text)
                ]
            ]
    }

type Ping = PacketWith Escher.Long

type Pong = PacketWith Escher.Long

pong :: Escher.Long -> Pong
pong n = Packet 0x01 n
