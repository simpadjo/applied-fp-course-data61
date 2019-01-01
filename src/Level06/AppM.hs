{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level06.AppM
  ( AppM(..)
  , App
  , liftEither
  , runApp
  ) where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Bifunctor         (Bifunctor (..))

import           Level06.Types          (Error)

-- We're going to upgrade the capability of our AppM by generalising the type of the errors that
-- it handles. This means that we'll be able to reuse our 'AppM' in more places that maybe have an
-- overabundance of 'IO (Either e a)' types.
--
-- Our new 'AppM'' will also use the record syntax to define our 'runAppM' function. This is a more
-- common definition of this kind of newtype.
--
newtype AppM e a = AppM
  { runAppM :: IO (Either e a)
  }

-- Predominantly our application has only one error type: 'Error'. It would be tedious to have to
-- declare that on every signature. We're able to use a type _alias_ to avoid this problem. We can
-- define this type alias          --in cb response
-- to make the error type variable concrete as 'Error'.

type App = AppM Error

-- We need to refactor the 'runAppM' function as now the name conflicts, and it needs to suit the
-- specialised 'App' type. The definition is even simpler than before. If someone near you is up to
-- the same section, try to explain to each other why this works.
--
runApp :: App a -> IO (Either Error a)
runApp = runAppM

-- You may copy your previously completed AppM instances here and then refactor them to suit the
-- more generalised type of AppM.

-- | -----------------------------------------------------------------------------------------------
-- | Copy from previous level and refactor, or reimplement to practice. The choice is yours.
-- | -----------------------------------------------------------------------------------------------

instance Functor (AppM e) where
  fmap :: (a -> b) -> AppM e a -> AppM e b
  fmap f (AppM x) = AppM r where
                      r = do
                           ee <- x
                           return $ f <$> ee

instance Applicative (AppM e) where
  pure :: a -> AppM e a
  pure  x = AppM $ pure $ Right x

  (<*>) :: AppM e (a -> b) -> AppM e a -> AppM e b
  (<*>) (AppM iof) (AppM iox) =
    AppM y where
      y = do
            ex <- iox
            ef <- iof
            return $ ef <*> ex

instance Monad (AppM e) where
  return :: a -> AppM e a
  return x = AppM $ pure $ Right x

  (>>=) :: AppM e a -> (a -> AppM e b) -> AppM e b
  (>>=)  (AppM iox) f =
    AppM y where
      y = do
            ex <- iox
            case ex of
              Left err -> return $ Left err
              Right x -> runAppM $ f x

instance MonadIO (AppM e) where
  liftIO :: IO a -> AppM e a
  liftIO ia = AppM y where
               y = do x <- ia
                      return $ Right x

instance MonadError e (AppM e) where
  throwError :: e -> AppM e a
  throwError e = AppM $ pure $ Left e

  catchError :: AppM e a -> (e -> AppM e a) -> AppM e a
  catchError (AppM r) f = AppM y where
                            y = do
                              e <- r
                              case e of
                                Left err -> runAppM $ f err
                                Right res -> return $ Right res

-- The 'Bifunctor' instance for 'Either' has proved useful several times
-- already. Now that our 'AppM' exposes both type variables that are used in our
-- 'Either', we can define a Bifunctor instance and reap similar benefits.
instance Bifunctor AppM where
  bimap :: (e -> d) -> (a -> b) -> AppM e a -> AppM d b
  bimap f g (AppM x) = AppM y where
                          y = bimap f g <$> x

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither :: Either e a -> AppM e a
liftEither (Left err) = throwError err
liftEither (Right res) = pure res
