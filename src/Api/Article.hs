{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Article where

import Control.Monad.IO.Class      ( liftIO )
import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Maybe
import qualified Data.Text as T
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.), deleteWhere, entityKey
                                   , insert, selectFirst, selectList, toSqlKey, fromSqlKey )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type ArticleAPI =
         "articles" :> Get '[JSON] [Entity Article]

    :<|> "articles" :> Capture "userId" Int64
                    :> ReqBody '[JSON] NewArticle
                    :> PostCreated '[JSON] ()

articleServer :: ServerT ArticleAPI App
articleServer = getArticles
           :<|> createArticle

getArticles :: App [Entity Article]
getArticles = runDb (selectList [] [])

createArticle :: Int64 -> NewArticle -> App ()
createArticle userId a = do
    time <- liftIO getCurrentTime
    runDb $
        insert (Article (slugify $ title a) (title a) (body a) (description a)
                         time Nothing (toSqlKey userId))
    return ()

--------------------------------------------------------------------------------
--  Utils

-- | TODO: move to utils file and add tests
slugify :: Text -> Text
slugify = T.intercalate "-" . T.words
