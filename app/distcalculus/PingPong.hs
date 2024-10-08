{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module PingPong where

import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)
import Data.Binary
import Data.Typeable
import GHC.Generics

data Protocol = Init {masterPid :: ProcessId, pongPid :: ProcessId }
              | Ping ProcessId
 	      | Pong
	      | Done
	      deriving (Typeable, Generic, Show)
	     
instance Binary Protocol

ping :: Process ()
ping = do
  logIt "ping process: started, awaiting init..."
  Init mPid pongPid <- expect
  myPid             <- getSelfPid
  logIt "ping process: sending Ping message"
  send pongPid (Ping myPid)
  logIt "ping process: waiting for Pong"
  Pong              <- expect
  logIt "ping process: Done"
  send mPid Done

pong :: Process ()
pong = do
  logIt "pong process: started, waiting for Ping..."
  Ping pingPid <- expect
  logIt "pong process: sending Pong"
  send pingPid Pong

master :: Process ()
master = do
  myPid <- getSelfPid
  logIt "master: spawn ping & pong"
  pingPid <- spawnLocal ping
  pongPid <- spawnLocal pong
  logIt "master: send Init"
  _       <- send pingPid (Init myPid pongPid)
  Done    <- expect
  logIt "master: Done"
  return ()

program :: IO ()
program = do
  Right t <- createTransport (defaultTCPAddr "localhost" "10501") defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node master

logIt :: String -> Process ()
logIt msg = liftIO . putStrLn $ msg