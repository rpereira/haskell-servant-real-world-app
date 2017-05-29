module Main where

import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp    (run)
import System.Environment          (lookupEnv)

import Api                         (app)
import Config                      (Config (..), Environment (..),
                                    makePool, setLogger)
import DB                          (runMigrations)
import Safe                        (readMay)

-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8080
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool runMigrations pool
    run port $ logger $ app cfg

-- | Looks up a setting in the environment, with a provided default, and 'read's
-- that information into the inferred type.
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
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
