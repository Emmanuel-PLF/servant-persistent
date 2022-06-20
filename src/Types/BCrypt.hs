{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Types.BCrypt
  ( BCrypt,
    hashPassword,
    --    validatePassword,
  )
where

-- Prelude.
--import           ClassyPrelude        hiding (hash)

-- Local imports.
import Control.Monad.Except (MonadIO, liftIO)
import qualified Crypto.KDF.BCrypt as BC
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist.MongoDB (PersistField)
import Database.Persist.Sql as S
import Database.Persist.TH ()
-- import Katip
--   ( KatipContext,
--     Severity (..),
--     logStr,
--     logTM,
--   )
import Logger ()

--------------------------------------------------------------------------------

-- | Newtype wrapper for passwords hashed using BCrypt.
newtype BCrypt = BCrypt
  { unBCrypt :: Text
  }
  deriving (Eq, Read, PersistField, S.PersistFieldSql, FromJSON, ToJSON, Show)

--------------------------------------------------------------------------------

-- | Produce a hashed output, given some plaintext input.
hashPassword :: MonadIO m => Text -> m BCrypt
hashPassword pass =
  let hash = liftIO $ BC.hashPassword 12 $ encodeUtf8 pass
   in BCrypt . decodeUtf8 <$> hash

-- | Validate that the plaintext is equivalent to a hashed @BCrypt@, log any
-- validation failures.

--validatePassword ::
--  (MonadReader r m, KatipContext m) =>
--  Text ->
--  BCrypt ->
--  m Bool
--validatePassword pass' hash' = do
--  let pass = encodeUtf8 pass'
--      hash = encodeUtf8 . unBCrypt $ hash'
--      isValid = BC.validatePasswordEither pass hash
--  case isValid of
--    Left e ->
--      logMsg "Password validation failed with [[ "
--            <> logStr e
--            <> " ]]"
--        pure
--        False
--    Right v -> pure v
