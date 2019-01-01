{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery), Query(..))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM(..))
import           Level05.DB.Types

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB action ioa = AppM y where
                 y = do
                       resp <- Sql.runDBAction ioa
                       let resp1 =  first DBError resp
                       return $ resp1 >>= action
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments (FirstAppDB conn) topic =
  let sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?" in
  let q = Sql.query conn (Query sql) (Sql.Only $ getTopic topic) :: (IO [DBComment]) in
  let parse = traverse fromDBComment :: [DBComment] -> Either Error [Comment] in
  runDB parse q

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic (FirstAppDB conn) topic text =
    let
      sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    in
      do
        time <- liftIO getCurrentTime
        let q = Sql.execute conn sql (getTopic topic, getCommentText text, time)
        runDB Right q

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics (FirstAppDB conn) =
    let
      sql = "SELECT DISTINCT topic FROM comments" :: Text
    in
        let q = Sql.query_ conn (Query sql) :: IO [Sql.Only Text] in
        runDB (traverse $ mkTopic . Sql.fromOnly) q

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic  (FirstAppDB conn) topic =
    let
      sql = "DELETE FROM comments WHERE topic = ?"
    in
    do
      let q = Sql.execute conn (Query sql) (Sql.Only $ getTopic topic)
      runDB Right q
-- Go to 'src/Level05/Core.hs' next.
