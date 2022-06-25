{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Init where

import Api (app)
import Api.User (writeSwaggerJSON)
import qualified Config as C
import Control.Concurrent (killThread)
import Control.Exception.Safe
import Control.Monad.Logger ()
--import qualified Control.Monad.Metrics as M
import qualified Data.Aeson as A
--import Database.Persist.Postgresql (runSqlPool)
import Data.Maybe (fromMaybe)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as Text
--import Data.Typeable (typeOf)
--import Models (doMigrations)
--import           Control.Monad.Trans      (liftIO)
import qualified Data.Yaml as Yaml
import qualified Katip
import qualified Logger as L
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Metrics (metrics, registerWaiMetrics)
import Safe (readMay)
import Say (say)
import System.Environment (lookupEnv)

--import System.Remote.Monitoring (forkServer, serverMetricStore, serverThreadId)

data ConfigApp = ConfigApp
  { cLogger :: L.Config,
    cServer :: C.ConfigApp
    -- , cDatabase   :: Database.Config
  }

instance Monoid ConfigApp where
  mempty =
    ConfigApp
      { cLogger = mempty,
        cServer = mempty
        --, cDatabase   = mempty
      }

instance Semigroup ConfigApp where
  l <> r =
    ConfigApp
      { cLogger = cLogger l <> cLogger r,
        cServer = cServer l <> cServer r
        --, cDatabase   = cDatabase   l <> cDatabase   r
      }

instance A.FromJSON ConfigApp where
  parseJSON = A.withObject "FromJSON Fugacious.Main.Server.Config" $ \o ->
    ConfigApp
      <$> o A..:? "logger" A..!= mempty
      <*> o A..:? "server" A..!= mempty

-- <*> o A..:? "database"    A..!= mempty

-- An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit

runAppDevel :: FilePath -> IO ()
runAppDevel f = do
  say "in runAppDevel"
  withConfig f $ \config -> do
    say "acquired config"
    cfg <-
      initialize config
        `finally` say "exited: initialize config"
    say "post-initialize"
    run (C.configPort config) cfg
      `finally` say "server is closed"

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize ::
  -- |
  C.Config ->
  IO Application
initialize cfg = do
  say "initialize"
  --waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
  --say "wai metrics - oups !"
  let logger = C.setLogger (C.configEnv cfg)
  say "run migrations"
  bracket
    (say "starting to run migrations")
    (\_ -> say "migrations complete")
    $ \_ -> do
      say "actually running no migrations with mongoDB"
      --runSqlPool doMigrations (configPool cfg) `catch` \(SomeException e) -> do
      --  say $
      --    mconcat
      --      [ "exception in doMigrations, type: ",
      --        tshow (typeOf e),
      --        ", shown: ",
      --        tshow e
      --      ]
      --  throwIO e
      say "okay all done"

  --say "generate Swagger"
  --writeSwaggerJSON
  say "making app"
  pure . logger . app $ cfg -- metrics (C.configWaiMetrics cfg) . app $ cfg

withConfig :: FilePath -> (C.Config -> IO a) -> IO a
withConfig f action = do
  say "acquireConfig"
  --port <- lookupSetting "PORT" 8081

  env <- lookupSetting "ENV" C.Development
  say $ "on env: " <> tshow env
  errOrConfig <- Yaml.decodeFileEither f
  ConfigApp {..} <- either (fail . show) return errOrConfig
  let port = fromMaybe 8000 $ C.cPort cServer
  say $ "on port:" <> tshow port
  bracket L.defaultLogEnv (\x -> say "closing katip scribes" >> Katip.closeScribes x) $ \logEnv -> do
    say "got log env"
    !pool <- C.makePool env logEnv `onException` say "exception in makePool"
    say "got pool "
    --bracket (forkServer "localhost" 8083) (\x -> say "closing ekg" >> do killThread $ serverThreadId x) $ \ekgServer -> do
    --  say "forked ekg server"
    --  let store = serverMetricStore ekgServer
    --  waiMetrics <- registerWaiMetrics store `onException` say "exception in registerWaiMetrics"
    --  say "registered wai metrics"
    --  metr <- M.initializeWith store
    --  say "got metrics"
    action
      C.Config
        { configPool = pool,
          configEnv = env,
          --configMetrics = metr,
          --configWaiMetrics = waiMetrics,
          configLogEnv = logEnv,
          configPort = port
          --configEkgServer = serverThreadId ekgServer
        }

-- | Takes care of cleaning up 'Config' resources
shutdownApp :: C.Config -> IO ()
shutdownApp cfg = do
  Katip.closeScribes (C.configLogEnv cfg)
  Pool.destroyAllResources (C.configPool cfg)
  -- Monad.Metrics does not provide a function to destroy metrics store
  -- so, it'll hopefully get torn down when async exception gets thrown
  -- at metrics server process
  --killThread (C.configEkgServer cfg)
  pure ()

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing ->
      return def
    Just str ->
      maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
        mconcat
          [ "Failed to read [[",
            str,
            "]] for environment variable ",
            env
          ]

tshow :: Show a => a -> Text
tshow = Text.pack . show
