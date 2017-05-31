{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Comment where

import Control.Monad.IO.Class      (liftIO)
import Data.Int                    (Int64)
import Data.Time                   (getCurrentTime)
import Database.Persist.Postgresql (Entity (..), deleteWhere, insert, rawSql,
                                    selectFirst, toPersistValue, toSqlKey,
                                    (==.))
import Database.Persist.Types      (SelectOpt (..))
import Servant

import Config                      (App (..))
import DB                          (runDb)
import Models.Article
import Models.Comment
import Models.User
import Types

type CommentAPI =
         "articles" :> Capture "slug" Slug
                    :> "comments"
                    :> Get '[JSON] (Cmts [Entity Comment])

    :<|> "articles" :> Capture "slug" Slug
                    :> "comments"
                    :> Capture "userId" Int64
                    :> ReqBody '[JSON] (Cmt NewComment)
                    :> PostCreated '[JSON] (Cmt (Maybe (Entity Comment)))

    :<|> "articles" :> Capture "slug" Slug
                    :> "comments"
                    :> Capture "articleId" Int64
                    :> DeleteNoContent '[JSON] NoContent

commentServer :: ServerT CommentAPI App
commentServer =
         getCommentsForArticle
    :<|> insertCommentInArticle
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

-- | While authentication is not supported, let's pass the userId as an
-- argument as well.
insertCommentInArticle :: Slug -> Int64 -> Cmt NewComment -> App (Cmt (Maybe (Entity Comment)))
insertCommentInArticle slug userId (Cmt newComment) = do
    maybeArticle <- runDb $ selectFirst [ArticleSlug ==. slug] []
    case maybeArticle of
      Nothing -> throwError err404
      Just article -> do
          comment <- insertComment newComment (toSqlKey userId) (entityKey article)
          return $ Cmt comment

insertComment :: NewComment -> Key User -> Key Article -> App (Maybe (Entity Comment))
insertComment comment userKey articleKey = do
    time <- liftIO getCurrentTime
    runDb $ do
        commentId <- insert $
            Comment (cmtBody comment) time Nothing userKey articleKey
        selectFirst [CommentId ==. commentId] []

deleteCommentInArticle :: Slug -> Int64 -> App NoContent
deleteCommentInArticle slug commentId = do
    maybeArticle <- runDb $ selectFirst [ArticleSlug ==. slug] []
    case maybeArticle of
      Nothing -> throwError err404
      Just article -> do
          runDb $ deleteWhere [ CommentArticleId ==. entityKey article
                              , CommentId ==. toSqlKey commentId ]
          return NoContent
