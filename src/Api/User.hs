{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Config (AppT (..))
import Control.Monad.Except (MonadIO, liftIO)
import Control.Monad.Logger (logDebugNS)
import Control.Monad.Metrics (increment, metricsCounters)
import qualified Control.Monad.Metrics as Metrics
import Data.Aeson.Encode.Pretty (encodePretty)
--import Servant.JS (vanillaJS, writeJSForAPI)

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.HashMap.Lazy (HashMap)
import Data.IORef (readIORef)
import Data.Int (Int64)
import Data.OpenApi hiding (Server)
import Data.Text (Text)
import Database.Persist.MongoDB
import Lens.Micro
import Models (User, runDb, userBio, userEmail, userImage, userName)
import qualified Models as Md
import Query.User (insertUser)
import Servant
  ( Capture,
    Get,
    HasServer (ServerT),
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    err404,
    throwError,
    type (:<|>) (..),
    type (:>),
  )
import Servant.OpenApi
import qualified System.Metrics.Counter as Counter
import Types.BCrypt (hashPassword)
import Types.User
  ( UserAddress (..),
    UserRegister,
    UserResponse (UserResponse),
    UserResponseBis (UserResponseBis),
  )

type UserAPI =
  "users" :> Get '[JSON] [UserResponse]
    :<|> "users"
      :> Capture "name" Text
      :> Get '[JSON] UserResponseBis
    :<|> "register"
      :> ReqBody '[JSON] UserRegister
      :> Post '[JSON] UserResponse

--    :<|> "metrics"
--      :> Get '[JSON] (HashMap Text Int64)

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = allUsers :<|> singleUser :<|> createUser -- :<|> waiMetrics

-- | Returns all users in the database.
allUsers :: MonadIO m => AppT m [UserResponse]
allUsers = do
  increment "allUsers"
  logDebugNS "web" "allUsers"
  au <- runDb (selectList [] [])
  mapM mkUserResponse au

-- | Returns a user by name or throws a 404 error.
singleUser :: MonadIO m => Text -> AppT m UserResponseBis
singleUser str = do
  increment "singleUser"
  logDebugNS "web" "singleUser"
  maybeUser <- runDb (selectFirst [Md.UserName ==. str] [])
  case maybeUser of
    Nothing ->
      throwError err404
    Just (Entity personId person) ->
      return
        ( UserResponseBis
            (userName person)
            (UserAddress "502" "la mutte" 49740)
        )

mkUserResponse :: (MonadIO m) => Entity User -> AppT m UserResponse
mkUserResponse (Entity pId per) = do
  return
    ( UserResponse
        (userEmail per)
        (userName per)
        (userBio per)
        (userImage per)
    )

-- | Creates a user in the database.
createUser :: MonadIO m => UserRegister -> AppT m UserResponse
createUser userReg = do
  increment "createUser"
  logDebugNS "web" "creating a user"
  hashedPw <- hashPassword $ userReg ^. password
  newUser <- runDb $ insertUser (userReg ^. name) (userReg ^. email) hashedPw
  logDebugNS "web" "after insert user"
  mkUserResponse newUser

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
  increment "metrics"
  logDebugNS "web" "metrics"
  metr <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)

-- | Generates JavaScript to query the User API.
-- generateJavaScript :: IO ()
-- generateJavaScript =
--  writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"

-- | Swagger spec for Todo API.
userSwagger :: OpenApi
userSwagger =
  toOpenApi userApi
    & info . title .~ "User API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an API that tests swagger integration"
    & info . license ?~ ("EPU" & url ?~ URL "http://...")
    & servers .~ ["http://localhost:8081" & description ?~ "super server test"]

writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty userSwagger)
