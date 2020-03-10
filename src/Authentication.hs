{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Authentication
  (HasUserDatabaseRef(..)
  , UserDatabase
  , defaultUserDatabase
  , ProtoUser(..)
  , Username(..)
  , AuthToken -- never export constructors!
  , Password(..)
  , AuthenticateResult(..)
  , authenticate
  , changePasswordForUser
  , ChangePasswordResult(..)
  , addUser
  , AddUserResult(..)
  , classifyUser
  , UserClassification(..)
  , useAuthToken
  , getUser
  , GetUserResult(..)
  , checkSame
  )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Aeson (FromJSON, ToJSON)
import RIO
import Servant.API (FromHttpApiData)

import Theory.Named (type (~~), name)
import Theory.Equality (type (==))
import Logic.Propositional (type (||))
import Logic.Proof (Proof, axiom)
import Data.Refined (type (:::))
import Data.The

newtype Username = Username Text deriving (Eq, Ord, Show, FromJSON, ToJSON, FromHttpApiData) via Text
newtype PasswordHash = PasswordHash Text deriving (Eq, FromJSON, ToJSON) via Text
newtype PasswordSalt = PasswordSalt Text deriving (Eq, FromJSON, ToJSON) via Text
newtype Password = Password Text deriving (Eq, FromJSON, ToJSON, FromHttpApiData) via Text
data Role = Admin | Normaluser deriving (Eq, Generic, FromJSON, ToJSON)

data User = User
    { username :: Username
    , passwordHash :: PasswordHash
    , passwordSalt :: PasswordSalt
    , isAdmin :: Role
    } deriving (Generic, FromJSON, ToJSON)

data ProtoUser = ProtoUser
    { protoUsername :: Username
    , protoPassword :: Password
    , protoIsAdmin :: Role
    } deriving (Generic, FromJSON, ToJSON)

newtype UserDatabase = UserDatabase (Map Username User)

class HasUserDatabaseRef e where
  userDbRef :: Lens' e (TVar UserDatabase)

defaultUserDatabase :: UserDatabase
defaultUserDatabase = UserDatabase $ Map.fromList
  [ (Username "admin",
     User
      (Username "admin")
      (PasswordHash "admin")
      (PasswordSalt "")
      Admin
      )]

data AddUserResult = AUOk | AUUserAlreadyPresent
addUserInternal :: (HasUserDatabaseRef env) => ProtoUser -> RIO env AddUserResult
addUserInternal newProtoUser = do
  dbRef <- view userDbRef
  atomically $ do
    userDbWrap <- readTVar dbRef
    let (UserDatabase userDb) = userDbWrap
    case Map.member (protoUsername newProtoUser) userDb of
        True -> return AUUserAlreadyPresent
        False -> do
          let newUser = User { username = protoUsername newProtoUser
                             , passwordSalt = (PasswordSalt "")
                             , passwordHash = (getHashedPassword (protoPassword newProtoUser) (PasswordSalt ""))
                             , isAdmin = protoIsAdmin newProtoUser
                             }
          let newDb = Map.insert (username newUser) newUser userDb
          writeTVar dbRef (UserDatabase newDb)
          return AUOk

data ChangePasswordResult = CPOk | CPNoSuchUser
changePasswordInternal :: (HasUserDatabaseRef env) => Username -> PasswordSalt -> PasswordHash -> RIO env ChangePasswordResult
changePasswordInternal name salt hash = do
  dbRef <- view userDbRef
  atomically $ do
    userDbWrap <- readTVar dbRef
    let (UserDatabase userDb) = userDbWrap
    case Map.member name userDb of
        False -> return CPNoSuchUser
        True -> do
          let newDb = Map.alter (fmap f) name userDb
          writeTVar dbRef (UserDatabase newDb)
          return CPOk
  where
  f user = user { passwordHash = hash, passwordSalt = salt }


-- | Not for production! Really use the salt!
getHashedPassword :: Password -> PasswordSalt -> PasswordHash
getHashedPassword (Password pw) _ = PasswordHash pw

data AuthenticateResult t = ARNoSuchUser | ARPasswordDoesntMatch | AROk t

authenticate
  :: (HasUserDatabaseRef env)
  => Username
  -> Password
  -> RIO env (AuthenticateResult AuthToken)
authenticate user pw = do
  userDbWrap <- readTVarIO =<< view userDbRef
  let (UserDatabase userDb) = userDbWrap
  case Map.member user userDb of
    False -> return ARNoSuchUser
    True -> do
      let userRecord = userDb Map.! user
      if (getHashedPassword pw (passwordSalt userRecord)) /= (passwordHash userRecord)
        then return ARPasswordDoesntMatch
        else return (AROk (AuthToken userRecord))

useAuthToken
  :: AuthToken
  -> (forall name. User ~~ name -> t)
  -> t
useAuthToken (AuthToken user) f = name user f

-- | Only ever export the Type, not the constructor, we need to be able to
-- trust that User value!
data AuthToken = AuthToken User

data IsAdmin xs
data IsNormalUser xs
data UserClassification u xs
  = IsAdmin (Proof (IsAdmin xs)) 
  | IsNormalUser (Proof (IsNormalUser xs))


-- | Consumers of our api can't name a user themselfs, so we
-- should be able to trust the record.
classifyUser
  :: User ~~ name
  -> UserClassification User xs
classifyUser user =
  case isAdmin (the user) of
    Admin -> IsAdmin axiom
    Normaluser -> IsNormalUser axiom

addUser
  :: (HasUserDatabaseRef env)
  => ProtoUser
  -> User ~~ name ::: IsAdmin name
  -> RIO env AddUserResult
addUser newUser _ = addUserInternal newUser

-- | Not for production come up with a good random source for salts.
changePasswordForUser
  :: (HasUserDatabaseRef env)
  => User ~~ name
  -> Password
  -> User ~~ name2 ::: IsAdmin name2 || name == name2
  -> RIO env ChangePasswordResult
changePasswordForUser user pw _ = changePasswordInternal (username (the user)) (PasswordSalt "") (getHashedPassword pw (PasswordSalt ""))


data GetUserResult t = GUOk t | GUNoSuchUser
getUser
  :: (HasUserDatabaseRef env)
  => Username
  -> (forall name. User ~~ name -> t)
  -> RIO env (GetUserResult t)
getUser user f = do
  userDbWrap <- readTVarIO =<< view userDbRef
  let (UserDatabase userDb) = userDbWrap
  case Map.member user userDb of
        False -> return GUNoSuchUser
        True ->  return (GUOk (name (userDb Map.! user) f))

checkSame :: User ~~ name -> User ~~ name2 -> Maybe (Proof (name == name2))
checkSame user1 user2 = if (username (the user1)) == (username (the user2)) then (Just axiom) else Nothing
