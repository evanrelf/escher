{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Escher where

import Control.Monad ((>=>))
import Control.Monad.Except (MonadError (..), liftEither, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.State.Strict (MonadState (..), evalStateT)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Escher.Packets (Handshake, StatusRequest)
import Network.Run.TCP (runTCPServer)

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as Network

type MonadEscher m =
  ( MonadIO m
  , MonadReader Network.Socket m
  , MonadState ByteString m
  , MonadError String m
  )

main :: IO ()
main = do
  putStrLn "Listening on port 3000"
  runTCPServer (Just "127.0.0.1") "3000" \socket ->
    escher
      & flip runReaderT socket
      & flip evalStateT mempty
      & (runExceptT >=> either fail pure)

escher :: MonadEscher m => m ()
escher = do
  receive @Handshake >>= \handshake -> liftIO do
    putStrLn "%%% Successful parsed handshake:"
    print handshake
    putStr "\n\n"

  receive @StatusRequest >>= \statusRequest -> liftIO do
    putStrLn "%%% Successful parsed status request:"
    print statusRequest
    putStr "\n\n"

receive :: forall a m . MonadEscher m => Cereal.Serialize a => m a
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
