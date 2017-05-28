{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Article where

import Control.Monad.IO.Class      ( liftIO )
import Data.Aeson                  ( FromJSON )
import Data.Char                   ( isAlphaNum )
import Data.Int                    ( Int64 )
import Data.Maybe                  ( Maybe, fromMaybe )
import qualified Data.Text as T
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.), deleteWhere, entityKey
                                   , insert, selectFirst, selectList, toSqlKey, deleteWhere )
import Database.Persist.Types      ( SelectOpt (..) )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type ArticleAPI =
         "articles" :> QueryParam "limit" Limit
                    :> QueryParam "offset" Offset
                    :> Get '[JSON] (Arts [Entity Article])

    :<|> "articles" :> Capture "slug" Slug
                    :> Get '[JSON] (Art (Maybe (Entity Article)))

    :<|> "articles" :> Capture "userId" Int64
                    :> ReqBody '[JSON] (Art NewArticle)
                    :> PostCreated '[JSON] ()

    :<|> "articles" :> Capture "slug" Slug
                    :> DeleteNoContent '[JSON] NoContent

articleServer :: ServerT ArticleAPI App
articleServer = getArticles
           :<|> getArticle
           :<|> createArticle
           :<|> deleteArticle

-- | TODO: implement required query params
getArticles :: Maybe Limit -> Maybe Offset -> App (Arts [Entity Article])
getArticles mbLimit mbOffset = do
    let limit = fromMaybe 20 mbLimit
        offset = fromMaybe 0 mbOffset
    articles <- runDb $ selectList [] [ LimitTo limit
                                      , OffsetBy offset
                                      , Desc ArticleCreatedAt ]
    return $ Arts articles (length articles)

getArticle :: Slug -> App (Art (Maybe (Entity Article)))
getArticle slug = do
    article <- runDb $ selectFirst [ArticleSlug ==. slug] []
    return $ Art article

-- | TODO: return an article
createArticle :: Int64 -> Art NewArticle -> App ()
createArticle userId (Art a) = do
    time <- liftIO getCurrentTime
    runDb $
        insert (Article (slugify $ title a) (title a) (body a) (description a)
                         time Nothing (toSqlKey userId))
    return ()

-- | TODO: delete everything associated with an article
deleteArticle :: Slug -> App NoContent
deleteArticle slug = do
    runDb $ deleteWhere [ArticleSlug ==. slug]
    return NoContent

--------------------------------------------------------------------------------
--  Utils

-- | TODO: move to utils file and add tests
slugify :: Text -> Slug
slugify text = T.intercalate "-" $ getSlugWords text

-- | Convert 'Text' to a possibly empty collection of words. Every word is
-- guaranteed to be non-empty alpha-numeric lower-cased sequence of
-- characters.
getSlugWords :: Text -> [Text]
getSlugWords = T.words . T.toLower . T.map f . T.replace "'" ""
    where f x = if isAlphaNum x then x else ' '
