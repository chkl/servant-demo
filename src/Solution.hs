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
import Authentication

-- let's define the API type
type Api = "meetup" :> (GetAllMeetups :<|> GetMeetupById)

type GetAllMeetups = Get '[JSON] [Meetup]

type GetMeetupById = Capture "mid" MeetupId :> Get '[JSON] (Maybe Meetup)

--
--
--
--
--
--

-- authCheck :: UserDB -> BasicAuthCheck Userinfo
-- authCheck db = BasicAuthCheck $ \(BasicAuthData username passwordArg) -> STM.atomically $ do
--   users <- STM.readTVar db
--   let mUser = Map.lookup (Username (Text.decodeUtf8 username)) users
--       checkedEntry = (\x -> (info x, Text.encodeUtf8 (fromPassword (password x)) == passwordArg)) <$> mUser
--   case checkedEntry of
--     Nothing -> return Unauthorized
--     Just (_, False) -> return Unauthorized
--     Just (info, True) -> return (Authorized info)
-- 
-- basicAuthServerContext :: UserDB -> Context (BasicAuthCheck Userinfo ': '[])
-- basicAuthServerContext db = authCheck db :. EmptyContext
-- 
-- app1 :: UserDB -> Application
-- app1 db = serveWithContext helloAPI (basicAuthServerContext db) (server db)



myServer :: (HasDatabaseRef env, HasUserDatabaseRef env) => ServerT Api (RIO env)
myServer = hGetAllMeetups :<|> hGetMeetupById
  where
    hGetAllMeetups = Business.findAllMeetups
    hGetMeetupById = Business.findMeetupById

myServerHoisted :: (HasDatabaseRef env, HasUserDatabaseRef env) => env -> Server Api
myServerHoisted env = hoistServer (Proxy @Api) (runRIO env) myServer

mkApplication :: (HasDatabaseRef env, HasUserDatabaseRef env) => env -> Wai.Application
mkApplication env = serve (Proxy @Api) (myServerHoisted env)

getAllMeetups :: ClientM [Meetup]

getMeetupById :: MeetupId -> ClientM (Maybe Meetup)
getAllMeetups :<|> getMeetupById = client (Proxy @Api)

-- auxililary stuff

deriving instance FromHttpApiData MeetupId

deriving instance ToHttpApiData MeetupId
