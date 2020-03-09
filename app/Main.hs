{-# LANGUAGE LambdaCase #-}

module Main where

import Business
import Motivation
import Network.Wai.Handler.Warp as Warp
import RIO
import Solution
import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn "welcome"
  -- s create a fresh environment with an empty 'database'
  env <- AppEnv <$> newTVarIO defaultDatabase
  -- create a WAI application
  app <- getArgs >>= \case
    ["v1"] -> Motivation.mkApplication env
    ["v2"] -> return $ Solution.mkApplication env
    _ -> error "USAGE servant-demo [v1|v2]"
  -- run the WAI application
  Warp.run 8080 app

newtype AppEnv = AppEnv (TVar Database)

instance HasDatabaseRef AppEnv where
  dbRef = lens (\(AppEnv db) -> db) (\(AppEnv _) db -> AppEnv db)
