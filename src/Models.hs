{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Config (Config, configPool)
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson ()
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
--import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)

import Database.Persist
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH
import Say (say)
import Types.BCrypt (BCrypt)
import Types.Instances ()
import Types.User (UBio, UEmail, UImage, UName)

let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) {mpsGeneric = False}
 in share
      [mkPersist mongoSettings]
      [persistLowerCase|
User json 
    name  Text
    email Text
    bio   Text  Maybe 
    image Text  Maybe 
    uuid      UUID        
    deriving Eq Show

Password json 
    hash      BCrypt  
    user      UserId
    deriving Eq Show

Command json
    num         Int
    user        UserId
    description Text 
    UniqueCommandNum num
    deriving Eq Show
    
|]

--type DB a = SqlPersistT IO a
type DB a = Action IO a

--doMigrations :: SqlPersistT IO ()
--doMigrations = do
--  liftIO $ say "in doMigrations, running?"
--  runMigration migrateAll
--  liftIO $ say "already run"

runDb :: (MonadReader Config m, MonadIO m) => DB b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runMongoDBPool master query pool
