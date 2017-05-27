{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Article where

import Control.Monad.IO.Class      ( liftIO )
import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Maybe                  ( Maybe, fromMaybe )
import qualified Data.Text as T
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.), deleteWhere, entityKey
                                   , insert, selectFirst, selectList, toSqlKey, fromSqlKey )
import Database.Persist.Types      ( SelectOpt (..) )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type ArticleAPI =
         "articles" :> QueryParam "limit" Limit
                    :> QueryParam "offset" Offset
                    :> Get '[JSON] (Arts [Entity Article])

    :<|> "articles" :> Capture "userId" Int64
                    :> ReqBody '[JSON] NewArticle
                    :> PostCreated '[JSON] ()

articleServer :: ServerT ArticleAPI App
articleServer = getArticles
           :<|> createArticle

-- | TODO: implement required query params
getArticles :: Maybe Limit -> Maybe Offset -> App (Arts [Entity Article])
getArticles mbLimit mbOffset = do
    let limit = fromMaybe 20 mbLimit
        offset = fromMaybe 0 mbOffset
    articles <- runDb $ selectList [] [ LimitTo limit
                                      , OffsetBy offset
                                      , Desc ArticleCreatedAt ]
    return $ Arts articles (length articles)

-- | TODO: return an article
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
slugify :: Text -> Slug
slugify = T.intercalate "-" . T.words
