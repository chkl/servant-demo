{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Motivation where

import Business
import Data.Text.Lazy (fromStrict)
import Network.Wai (Application)
import qualified Network.Wreq as W
import RIO
import qualified RIO.Text as T
import RIO.Time
import Web.Scotty.Trans

-- a simple server
mkApplication :: (HasDatabaseRef env) => env -> IO Application
mkApplication db = scottyAppT (runRIO db) $ do
  get "/meetup" hFindAllMeetups
  get "/meetup/:id" hGetMeetup
  where
    hFindAllMeetups, hGetMeetup :: (HasDatabaseRef env) => ActionT MyErrorType (RIO env) ()
    hFindAllMeetups = lift findAllMeetups >>= json
    hGetMeetup = do
      mid <- param "id"
      mMeetup <- lift (findMeetupById mid)
      case mMeetup of
        Nothing -> raise NotFound
        Just m -> json m

data MyErrorType = NotFound | SomethingElse Text deriving (Show)

instance ScottyError MyErrorType where
  stringError = SomethingElse . T.pack
  showError NotFound = "not found"
  showError (SomethingElse t) = fromStrict t

-- a simple client
getAllMeetups :: IO [Meetup]
getAllMeetups = do
  x <- W.asJSON =<< W.get "localhost:3000/meetup"
  return (x ^. W.responseBody)

getMeetupById :: MeetupId -> IO Meetup
getMeetupById mid = do
  x <- W.asJSON =<< W.get ("localhost:3000/meetup/" <> show (toInt mid))
  return (x ^. W.responseBody)
