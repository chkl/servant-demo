{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Business where

import Control.Lens ((%~), (.~), use)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import RIO
import RIO.Time
import Web.Scotty

data Database
  = Database
      { _meetups :: Map.Map MeetupId Meetup,
        _meetupLastId :: MeetupId
      }

type Location = Text

class HasDatabaseRef e where
  dbRef :: Lens' e (TVar Database)

newtype MeetupId = MeetupId {toInt :: Int}
  deriving (Ord, Eq, Enum, ToJSON, FromJSON, Parsable) via Int

data Meetup
  = Meetup
      { meetupId :: MeetupId,
        title :: Text,
        location :: Location,
        date :: LocalTime
      }
  deriving (Generic, ToJSON, FromJSON)

makeLenses 'Database

meetupOne = Meetup (MeetupId 123) "Typsichere Webanwendungen mit Servant" "Spark" day
  where
    -- Could we have not have an IsString instance for LocalTime (or QuasiQuoter) ?
    day = parseTimeOrError True defaultTimeLocale (iso8601DateFormat (Just "%H:%M")) "2020-03-10 19:30"

-- 'business logic'
findAllMeetups :: (HasDatabaseRef env) => RIO env [Meetup]
findAllMeetups = do
  db <- readTVarIO =<< view dbRef
  return $ Map.elems (_meetups db)

findMeetupById :: (HasDatabaseRef env) => MeetupId -> RIO env (Maybe Meetup)
findMeetupById mid = do
  db <- readTVarIO =<< view dbRef
  return $ Map.lookup mid (_meetups db)

createMeetup :: (HasDatabaseRef env) => Text -> Location -> LocalTime -> RIO env Meetup
createMeetup title location time = do
  dbref <- view dbRef
  atomically $ do
    db <- readTVar dbref
    let mid = succ (db ^. meetupLastId)
    let newMeetup = Meetup mid title location time
    let db' =
          db
            & meetups %~ Map.insert mid newMeetup
            & meetupLastId .~ mid
    writeTVar dbref db'
    return newMeetup
