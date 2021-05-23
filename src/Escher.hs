{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Escher where

import Data.ByteString (ByteString)
import Escher.Packets (Handshake, StatusRequest)
import Network.Run.TCP (runTCPServer)

import qualified Data.Serialize as Cereal
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as Network

main :: IO ()
main = do
  putStrLn "Listening on port 3000"
  runTCPServer (Just "127.0.0.1") "3000" \socket -> do
    (result, rest) <- receive @Handshake socket Nothing

    case result of
      Left err ->
        putStrLn ("!!! Failed to parse handshake:\n" <> err)

      Right handshake -> do
        putStrLn "%%% Successful parsed handshake:"
        print handshake
        putStr "\n\n"

    (result, _rest) <- receive @StatusRequest socket (Just rest)

    case result of
      Left err ->
        putStrLn ("!!! Failed to parse status request:\n" <> err)

      Right statusRequest -> do
        putStrLn "%%% Successful parsed status request:"
        print statusRequest
        putStr "\n\n"

receive
  :: Cereal.Serialize a
  => Network.Socket
  -> Maybe ByteString
  -> IO (Either String a, ByteString)
receive socket = go (Cereal.runGetPartial Cereal.get)
  where
    go k start = do
      bytes <- case start of
        Just bytes -> pure bytes
        Nothing -> Network.recv socket 1024

      case k bytes of
        Cereal.Fail err rest -> pure (Left err, rest)
        Cereal.Done x rest -> pure (Right x, rest)
        Cereal.Partial k' -> go k' Nothing
