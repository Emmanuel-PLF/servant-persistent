{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Query.User where

-- Prelude.

-- Local imports.

import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
--import Database.Esqueleto

import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import Models
import Types.BCrypt
import Types.User

--------------------------------------------------------------------------------

-- | Insert a new user into the database.
insertUser :: Text -> Text -> BCrypt -> DB (Entity User)
insertUser uName uEmail uPass = do
  now <- liftIO getCurrentTime
  newUuid <- liftIO nextRandom

  userKey <- insert $ User uName uEmail Nothing Nothing newUuid

  _ <- insert $ Password uPass userKey

  --userRec <- get userKey
  pure (Entity userKey $ User uName uEmail Nothing Nothing newUuid)

--------------------------------------------------------------------------------
--getAllUsers :: DB [Entity User]
--getAllUsers =
--  select $
--    from $ \dbUser -> do
--      pure dbUser
--
---- | Retrieve a user and their hashed password from the database.
--getUserByEmail :: Text -> DB (Maybe (Entity User, Entity Password))
--getUserByEmail uEmail = fmap listToMaybe $
--  select $
--    from $ \(dbUser `InnerJoin` dbPass) -> do
--      on (dbUser ^. UserId ==. dbPass ^. PasswordUser)
--      where_ (dbUser ^. UserEmail ==. val uEmail)
--      pure (dbUser, dbPass)
--
---- | Retrieve a user and their hashed password from the database.
--getUserByUuid :: UUID -> DB (Maybe (Entity User))
--getUserByUuid uUuid = fmap listToMaybe $
--  select $
--    from $ \dbUser -> do
--      where_ (dbUser ^. UserUuid ==. val uUuid)
--      pure dbUser
