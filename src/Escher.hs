{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Escher where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT, MonadError (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT, evalStateT)
import Data.ByteString (ByteString)
import Escher.Packets
import Network.Run.TCP (runTCPClient, runTCPServer)

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as Network

main :: IO ()
main = server

client :: IO ()
client = runTCPClient "localhost" "25565" $ runEscher do
  send @Handshake $ Packet 0x00 HandshakeData
    { protocolVersion = 754
    , serverAddress = "localhost"
    , serverPort = 25565
    , nextState = 1
    }
  liftIO $ putStrLn "Sent handshake"

  send @StatusRequest (Packet 0x00 ())
  liftIO $ putStrLn "Sent status request"

  packet <- receive @Packet
  liftIO $ print packet

server :: IO ()
server = do
  putStrLn "Listening on port 3000"
  runTCPServer (Just "127.0.0.1") "3000" $ runEscher do
    _ <- receive @Handshake
    liftIO $ putStrLn "Received handshake"

    _ <- receive @StatusRequest
    liftIO $ putStrLn "Received status request"

    send @StatusResponse statusResponse
    liftIO $ putStrLn "Sent status response"

    Packet _ n <- receive @Ping
    liftIO $ putStrLn "Received ping"

    send @Pong (pong n)
    liftIO $ putStrLn "Sent pong"

    socket <- ask
    liftIO $ Network.shutdown socket Network.ShutdownBoth
    liftIO $ putStrLn "Closed socket"

runEscher
  :: ReaderT Network.Socket (StateT ByteString (ExceptT String IO)) a
  -> Network.Socket
  -> IO a
runEscher m socket
  = (either fail pure <=< runExceptT)
  . flip evalStateT (mempty :: ByteString)
  . flip runReaderT socket
  $ m

receive
  :: forall a m
   . MonadIO m
  => MonadReader Network.Socket m
  => MonadState ByteString m
  => MonadError String m
  => Cereal.Serialize a
  => m a
receive = do
  socket <- ask
  bytes <- get
  (result, rest) <- receive' socket (Just bytes)
  put rest
  liftEither (Bifunctor.first ("Failed to decode packet:\n" <>) result)

receive'
  :: forall a m
   . MonadIO m
  => Cereal.Serialize a
  => Network.Socket
  -> Maybe ByteString
  -> m (Either String a, ByteString)
receive' socket = liftIO . go (Cereal.runGetPartial Cereal.get)
  where
    go k maybeBytes = do
      bytes <- case maybeBytes of
        Just bytes | not . ByteString.null $ bytes ->
          pure bytes
        _ ->
          Network.recv socket 1024

      case k bytes of
        Cereal.Fail err rest -> pure (Left err, rest)
        Cereal.Done x rest -> pure (Right x, rest)
        Cereal.Partial k' -> go k' Nothing

send
  :: forall a m
   . MonadIO m
  => MonadReader Network.Socket m
  => Cereal.Serialize a
  => a
  -> m ()
send x = do
  socket <- ask
  send' socket x

send'
  :: forall a m
   . MonadIO m
  => Cereal.Serialize a
  => Network.Socket
  -> a
  -> m ()
send' socket x = liftIO $ Network.sendAll socket (Cereal.encode x)
