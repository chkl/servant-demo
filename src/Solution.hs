{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solution where

import Business
import qualified Network.Wai as Wai
import RIO
import Servant
import Servant.Client
import Authentication

-- let's define the API type
type MeetupApi = "meetup" :> (GetAllMeetups :<|> GetMeetupById)

type GetAllMeetups = Get '[JSON] [Meetup]

type GetMeetupById = Capture "mid" MeetupId :> Get '[JSON] (Maybe Meetup)

type UsersApi = BasicAuth "servant-demo" AuthToken :> "users" :> (
  AddNewUser :<|>
  ChangePassword
  )

type AddNewUser = ReqBody '[JSON] ProtoUser :> Post '[JSON] (Maybe Username)
type ChangePassword = "changepassword" :> Capture "username" Username :> Capture "password" Password :> Post '[JSON] (Maybe Username)

type Api = MeetupApi -- :<|> UsersApi 

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
myServerHoisted e = hoistServer (Proxy @Api) (runRIO e) myServer

mkApplication :: (HasDatabaseRef env, HasUserDatabaseRef env) => env -> Wai.Application
mkApplication e = serve (Proxy @Api) (myServerHoisted e)

-- auxililary stuff

deriving instance FromHttpApiData MeetupId

deriving instance ToHttpApiData MeetupId
