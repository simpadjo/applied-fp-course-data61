{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString, readFile)

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json, parseWaargonaut)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM(..), liftEither)
import           Level06.Types              (ConfigError (..),
                                             PartialConf (PartialConf), partialConfDecoder)
import           Control.Exception          (try)
import Debug.Trace
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile path = AppM y where
                      y = do
                            e <- try (Data.ByteString.readFile path) :: IO (Either IOError ByteString)
                            let e1 = first (const $ FileDoesntExist path) e
                            return e1


-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile path =
  do jsonStr <- readConfFile path
     let x = D.simpleDecode partialConfDecoder parseFunc jsonStr
     liftEither $ first (\(e , _ ) -> BadConfFile e) x
  where
    parseFunc :: ByteString -> Either DecodeError Json
    parseFunc = first (ParseFailed . pack . show) . AB.parseOnly parseWaargonaut

-- Go to 'src/Level06/Conf.hs' next.
