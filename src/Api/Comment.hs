{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Comment where

import Data.Maybe                  ( Maybe )
import Data.Text                   ( Text )
import Data.Time                   ( getCurrentTime )
import Database.Persist.Postgresql ( Entity (..), (==.), rawSql, toPersistValue )
import Database.Persist.Types      ( SelectOpt (..) )
import Servant

import Config                      ( App (..), Config (..) )
import Models
import Types

type CommentAPI =
         "articles" :> Capture "slug" Slug
                    :> "comments"
                    :> Get '[JSON] (Cmts [Entity Comment])

commentServer :: ServerT CommentAPI App
commentServer = getCommentsForArticle

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
