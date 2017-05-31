{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Comment where

import Data.Int                    (Int64)
import Database.Persist.Postgresql (Entity (..), deleteWhere, rawSql,
                                    selectFirst, toPersistValue, toSqlKey,
                                    (==.))
import Database.Persist.Types      (SelectOpt (..))
import Servant

import Config                      (App (..))
import DB                          (runDb)
import Models.Article
import Models.Comment
import Types

type CommentAPI =
         "articles" :> Capture "slug" Slug
                    :> "comments"
                    :> Get '[JSON] (Cmts [Entity Comment])

    :<|> "articles" :> Capture "slug" Slug
                    :> "comments"
                    :> Capture "articleId" Int64
                    :> DeleteNoContent '[JSON] NoContent

commentServer :: ServerT CommentAPI App
commentServer =
         getCommentsForArticle
    :<|> deleteCommentInArticle

-- | TODO: the returned object does not comply with the API spec. we need to
-- remove the article_id and add the field author with info about the user who
-- posted the comment.
getCommentsForArticle :: Slug -> App (Cmts [Entity Comment])
getCommentsForArticle slug = do
    comments <- runDb $ rawSql stm [toPersistValue slug]
    return (Cmts comments)
        where stm = "SELECT ?? FROM comments \
                    \WHERE article_id IN (\
                        \SELECT id FROM articles WHERE slug = ?)"

deleteCommentInArticle :: Slug -> Int64 -> App NoContent
deleteCommentInArticle slug commentId = do
    maybeArticle <- runDb $ selectFirst [ArticleSlug ==. slug] []
    case maybeArticle of
      Nothing -> throwError err404
      Just article -> do
          runDb $ deleteWhere [ CommentArticleId ==. entityKey article
                              , CommentId ==. toSqlKey commentId ]
          return NoContent
