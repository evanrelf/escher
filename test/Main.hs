{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Escher.Packets
import Escher.Types
import System.Exit (die)
import Test.Tasty.HUnit ((@?=))
import Prelude hiding (String)

import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
  [ test_serialize_types
  , test_serialize_packets
  ]


test_serialize_types :: Tasty.TestTree
test_serialize_types = Tasty.testGroup "Serialize types"
  [ HUnit.testCase "roundtrip string" do
      roundtrip @(String 42) $ String ""
      roundtrip @(String 42) $ String "\0"
      roundtrip @(String 42) $ String "hello world"
      roundtrip @(String 42) $ String "\n\n"

  , HUnit.testCase "roundtrip varint" do
      roundtrip @VarInt $ VarInt 42
      roundtrip @VarInt $ VarInt -42

  , HUnit.testCase "roundtrip varlong" do
      roundtrip @VarLong $ VarLong 42
      roundtrip @VarLong $ VarLong -42
  ]


test_serialize_packets :: Tasty.TestTree
test_serialize_packets = Tasty.testGroup "Serialize packets"
  [ HUnit.testCase "roundtrip handshake" do
      roundtrip @Handshake $ Packet 0x00 HandshakeData
        { protocolVersion = 756
        , serverAddress = String "localhost"
        , serverPort = 3000
        , nextState = 0x01
        }

  , HUnit.testCase "roundtrip status request" do
      roundtrip @StatusRequest $ Packet 0x00 ()

  , HUnit.testCase "roundtrip status response" do
      roundtrip @StatusResponse statusResponse

  , HUnit.testCase "roundtrip ping" do
      roundtrip @Ping $ Packet 0x01 42

  , HUnit.testCase "roundtrip pong" do
      roundtrip @Pong $ pong 42
  ]


decodeFile :: Cereal.Serialize a => FilePath -> IO a
decodeFile filepath = do
  bytes <- ByteString.readFile filepath
  either die pure $ Cereal.decode bytes


roundtrip
  :: Cereal.Serialize a
  => Eq a
  => Show a
  => a
  -> HUnit.Assertion
roundtrip x = Cereal.decode (Cereal.encode x) @?= Right x
