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
import Data.Text.Encoding (decodeUtf8)

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

authCheck :: (HasUserDatabaseRef env) => env -> BasicAuthCheck AuthToken
authCheck env = BasicAuthCheck $ \(BasicAuthData username passwordArg) -> do
  result <- runRIO env (authenticate
    (Username (decodeUtf8 username))
    (Password (decodeUtf8 passwordArg)))
  case result of
    ARNoSuchUser -> return NoSuchUser
    ARPasswordDoesntMatch -> return BadPassword
    AROk token -> return (Authorized token)

basicAuthServerContext :: (HasUserDatabaseRef env) => env -> Context (BasicAuthCheck AuthToken ': '[])
basicAuthServerContext env = authCheck env :. EmptyContext

myServer :: (HasDatabaseRef env, HasUserDatabaseRef env) => ServerT Api (RIO env)
myServer = hGetAllMeetups :<|> hGetMeetupById
  where
    hGetAllMeetups = Business.findAllMeetups
    hGetMeetupById = Business.findMeetupById

myServerHoisted :: (HasDatabaseRef env, HasUserDatabaseRef env) => env -> Server Api
myServerHoisted e = hoistServer (Proxy @Api) (runRIO e) myServer

mkApplication :: (HasDatabaseRef env, HasUserDatabaseRef env) => env -> Wai.Application
mkApplication e = serveWithContext (Proxy @Api) (basicAuthServerContext e) (myServerHoisted e)

-- auxililary stuff

deriving instance FromHttpApiData MeetupId

deriving instance ToHttpApiData MeetupId
