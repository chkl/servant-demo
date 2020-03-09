{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Business
import qualified Data.Aeson as JSON
import Motivation
import qualified Network.HTTP.Types as H
import RIO
import Test.Tasty
import Test.Tasty.Wai

newtype TestEnv = TestEnv (TVar Database)

instance HasDatabaseRef TestEnv where
  dbRef = lens (\(TestEnv db) -> db) (\(TestEnv _) db -> TestEnv db)

main :: IO ()
main = do
  env <- TestEnv <$> newTVarIO defaultDatabase
  app <- Motivation.mkApplication env
  defaultMain $
    testGroup
      "Tasty-Wai tests"
      [ testWai app "empty meetups" $ do
          res <- get "/meetup"
          assertStatus' H.status200 res
          assertContentType "application/json" res
          assertBody (JSON.encode ([] :: [Meetup])) res,
        testWai app "non-existent id" $ do
          res <- get "/meetup/42"
          assertStatus' H.status404 res,
        testWai app "create" $ do
          let proto = ProtoMeetup "foo" "bar" (unsafeParseTime "2021-01-01 10:33")
          res <- post "/meetup" (JSON.encode proto)
          assertStatus' H.status201 res
      ]
