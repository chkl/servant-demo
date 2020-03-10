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

import Data.Refined ((...))
import Logic.Propositional (introOrL, introOrR)

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

type Api = MeetupApi :<|> UsersApi 

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

hChangePassword :: (HasUserDatabaseRef env) => AuthToken -> Username -> Password -> RIO env (Maybe Username)
hChangePassword token username newPassword = useAuthToken token (\user -> do
  targetExists <- getUser username (\targetUser -> do
     case classifyUser user of
       (IsAdmin proof) -> do
         result <- changePasswordForUser targetUser newPassword (user ... (introOrL proof))
         case result of
           CPOk -> return (Just username)
           CPNoSuchUser -> return Nothing
       (IsNormalUser proof) -> case (checkSame targetUser user) of
         Nothing -> fail "you can only change your own password"
         (Just proof) -> do
           result <- changePasswordForUser targetUser newPassword (user ... (introOrR proof))
           case result of
             CPOk -> return (Just username)
             CPNoSuchUser -> return Nothing
     )
  case targetExists of
    GUNoSuchUser -> fail "target user doesn't exist"
    GUOk action -> action
  )

myServer :: (HasDatabaseRef env, HasUserDatabaseRef env) => ServerT Api (RIO env)
myServer = (hGetAllMeetups :<|> hGetMeetupById) :<|> (\token -> (hAddNewUser token :<|> hChangePassword token))
  where
    hGetAllMeetups = Business.findAllMeetups
    hGetMeetupById = Business.findMeetupById
    hAddNewUser token protoUser = useAuthToken token (\user ->
      case classifyUser user of
        (IsNormalUser _) -> fail "not allowed for normal user"
        (IsAdmin proof) -> do
          result <- addUser protoUser (user ... proof)
          case result of 
            AUUserAlreadyPresent -> fail "user already present"
            AUOk -> return (Just (protoUsername protoUser)))

myServerHoisted :: (HasDatabaseRef env, HasUserDatabaseRef env) => env -> Server Api
myServerHoisted e = hoistServerWithContext (Proxy @Api) (Proxy :: Proxy '[BasicAuthCheck AuthToken]) (runRIO e) myServer

mkApplication :: (HasDatabaseRef env, HasUserDatabaseRef env) => env -> Wai.Application
mkApplication e = serveWithContext (Proxy @Api) (basicAuthServerContext e) (myServerHoisted e)

-- auxililary stuff

deriving instance FromHttpApiData MeetupId

deriving instance ToHttpApiData MeetupId
