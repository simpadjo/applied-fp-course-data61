{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType(..), Error, RqType,
                                           mkCommentText, mkTopic,
                                           renderContentType,  RqType(..),
                                           Error(..))

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status ct text = responseLBS status [("Content-Type", renderContentType ct)] text

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 ct text = mkResponse status200 ct text

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 ct text = mkResponse status404 ct text

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 ct text = mkResponse status400 ct text

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest top comm = do
                          topic <- mkTopic top
                          comment <- mkCommentText $ lazyByteStringToStrictText comm
                          return $ AddRq topic comment
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest top = do
                          topic <- mkTopic top
                          return $ ViewRq topic

mkListRequest
  :: Either Error RqType
mkListRequest = Right $ ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse (Error e) = resp400 PlainText (LBS.fromStrict $ Data.Text.Encoding.encodeUtf8 e)

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case pathInfo req of
    [topic, "add"] -> do comment <- strictRequestBody req
                         return $ mkAddRequest topic comment
    [topic, "view"] -> return $ mkViewRequest topic
    ["list"] -> return $ mkListRequest
    _ -> return $ Left $ Error "Not found"

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right $ resp200 PlainText "add not implemented"
handleRequest (ViewRq _) = Right $ resp200 PlainText "view not implemented"
handleRequest ListRq = Right $ resp200 PlainText "list not implemented"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app req cb = do maybeReq <- mkRequest req
                case maybeReq of
                  Left err -> cb $ mkErrorResponse err
                  Right rq -> case handleRequest rq of
                    Left e -> cb $ mkErrorResponse e
                    Right resp -> cb resp

runApp :: IO ()
runApp = run 8080 app
