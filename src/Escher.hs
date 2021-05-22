{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Escher where

import Network.Run.TCP (runTCPServer)

import qualified Data.ByteString as ByteString
import qualified Network.Socket.ByteString as Network
import qualified System.IO as IO

main :: IO ()
main = do
  putStrLn "Listening on port 3000"
  runTCPServer (Just "127.0.0.1") "3000" \socket -> do
    let loop = do
          bytes <- Network.recv socket 1024
          ByteString.hPut IO.stdout bytes
          loop
    loop
