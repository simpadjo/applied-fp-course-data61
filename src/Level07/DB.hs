{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks, ask)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, Env (envDB), AppM(..), liftEither)

import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)
import            Level07.DB.Types                  (DBComment)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn =
  let env = ask :: App Env in
  fmap (dbConn . envDB ) env

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB fn cb = do conn <- getDBConn
                 a0 <- liftIO $ cb conn
                 liftEither $ fn a0

getComments
  :: Topic
  -> App [Comment]
getComments topic =
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?" in
  let parse = \dbs -> traverse fromDBComment dbs in
  runDB parse (\conn -> Sql.query conn q (Sql.Only . getTopic $ topic))

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic =
  error "Copy your completed 'appCommentToTopic' and refactor to match the new type signature"

getTopics
  :: App [Topic]
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDB (traverse ( mkTopic . Sql.fromOnly )) (\conn ->  Sql.query_ conn q)

deleteTopic
  :: Topic
  -> App ()
deleteTopic topic =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDB Right $ \conn ->  Sql.execute conn q (Sql.Only . getTopic $ topic)

-- Go on to 'src/Level07/Core.hs' next.
