{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Favorite where

import Data.Aeson                  ( FromJSON )
import Data.Int                    ( Int64 )
import Data.Maybe
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.), deleteWhere, entityKey
                                   , insert, selectFirst, toSqlKey )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type FavoriteAPI =
         "articles" :> Capture "slug" Slug
                    :> "favorite"
                    :> Post '[JSON] (Art (Entity Article))

    :<|> "articles" :> Capture "slug" Slug
                    :> "favorite"
                    :> Delete '[JSON] (Art (Entity Article))

favoriteServer :: ServerT FavoriteAPI App
favoriteServer = favoriteArticle
            :<|> unfavoriteArticle

-- TODO: We mock a specific user when marking an article as favorite - userId
-- should come from session.
favoriteArticle :: Slug -> App (Art (Entity Article))
favoriteArticle slug = do
    maybeArticle <- runDb $ selectFirst [ArticleSlug ==. slug] []
    case maybeArticle of
      Nothing -> throwError err404
      Just article -> do
          runDb $ insert (Favorite (toSqlKey 1) (entityKey article))
          return $ Art article

-- TODO: We mock a specific user when unmarking an article as favorite - userId
-- should come from session.
unfavoriteArticle :: Slug -> App (Art (Entity Article))
unfavoriteArticle slug = do
    maybeArticle <- runDb $ selectFirst [ArticleSlug ==. slug] []
    case maybeArticle of
      Nothing -> throwError err404
      Just article -> do
          runDb $ deleteWhere [ FavoriteUserId ==. toSqlKey 1
                              , FavoriteArticleId ==. entityKey article ]
          return $ Art article
