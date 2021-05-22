{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Escher where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Word (Word16)
import GHC.TypeLits (Nat)
import Network.Run.TCP (runTCPServer)
import Prelude hiding (length)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as Network
import qualified System.IO as IO

type LByteString = LByteString.ByteString

type MCUnsignedShort = Word16

data MCString (length :: Nat) = MCString
  { size :: MCVarInt
  , string :: Text
  }

type MCVarInt = Int32

type MCByteArray = LByteString

data Packet = Packet
  { length :: MCVarInt
  , packetId :: MCVarInt
  , data_ :: MCByteArray
  }

data Handshake = Handshake
  { protocolVersion :: MCVarInt
  , serverAddress :: MCString 255
  , serverPort :: MCUnsignedShort
  , nextState :: MCVarInt
  }

toPacket :: ByteString -> Either String Packet
toPacket bytes = undefined

toHandshake :: Packet -> Either String Handshake
toHandshake Packet{length, packetId, data_}
  | packetId /= 0x00 = Left $ unlines
      [ "Incorrect packet ID for handshake"
      , "Expected: 0x00"
      , "Received: " <> show packetId
      ]
  | otherwise = undefined

main :: IO ()
main = do
  putStrLn "Listening on port 3000"
  runTCPServer (Just "127.0.0.1") "3000" \socket -> do
    let loop = do
          bytes <- Network.recv socket 1024
          ByteString.hPut IO.stdout bytes
          loop
    loop
