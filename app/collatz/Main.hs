{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
-- import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable, runProcess, newLocalNode)
import Control.Monad (unless)
import Data.Functor (void)
import System.Environment (getArgs, getProgName)

collatz :: Int -> Int
collatz 1 = 1
collatz x
  | even x = x `div` 2
  | otherwise = 3 * x + 1

remotableDecl [ [d|
  distributedCollatz :: ([NodeId], Int) -> Process ()
  distributedCollatz (slaves, x) = do
    let x' = collatz x
        nextNode = slaves !! (x' `mod` length slaves)
    say $ show x ++ " goes to " ++ show x'
    unless (x' == 1) $
      void $ spawn nextNode $ $(mkClosure 'distributedCollatz) (slaves, collatz x)
  |]]

remoteTable = __remoteTableDecl initRemoteTable

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  distributedCollatz (slaves, 27)
  liftIO $ getLine
  terminateAllSlaves backend

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs

  case args of
    ["master", host, port] -> do
      -- backend <- initializeBackend host port remoteTable
      -- startMaster backend (master backend)
      
      backendNode <- newLocalNode transport initRemoteTable
      runProcess backendNode (master backendNode)
      
    ["slave", host, port] -> do
      -- backend <- initializeBackend host port remoteTable
      -- startSlave backend
      
      backendNode <- newLocalNode transport initRemoteTable
      runProcess backendNode (slave backendNode)
    _ ->
      backendNode <- newLocalNode transport initRemoteTable
      runProcess backendNode (master backendNode)
      putStrLn $ "usage: " ++ prog ++ " (master | slave) host port"