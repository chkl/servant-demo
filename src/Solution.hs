{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Solution where

import Business
import qualified Network.Wai as Wai
import RIO
import Servant
import Servant.Client

-- let's define the API type
type Api = "meetup" :> (GetAllMeetups :<|> GetMeetupById)

type GetAllMeetups = Get '[JSON] [Meetup]

type GetMeetupById = Capture "mid" MeetupId :> Get '[JSON] (Maybe Meetup)

--
--
--
--
--
myServer :: (HasDatabaseRef env) => ServerT Api (RIO env)
myServer = hGetAllMeetups :<|> hGetMeetupById
  where
    hGetAllMeetups = Business.findAllMeetups
    hGetMeetupById = Business.findMeetupById

myServerHoisted :: (HasDatabaseRef env) => env -> Server Api
myServerHoisted env = hoistServer (Proxy @Api) (runRIO env) myServer

mkApplication :: (HasDatabaseRef env) => env -> Wai.Application
mkApplication env = serve (Proxy @Api) (myServerHoisted env)

getAllMeetups :: ClientM [Meetup]

getMeetupById :: MeetupId -> ClientM (Maybe Meetup)
getAllMeetups :<|> getMeetupById = client (Proxy @Api)

-- auxililary stuff

deriving instance FromHttpApiData MeetupId

deriving instance ToHttpApiData MeetupId
