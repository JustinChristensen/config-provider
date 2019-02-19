{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State

data Options = Options { logger :: String -> IO (), getName :: String }
data ConfigNode = ConfigNode (Maybe String) deriving (Show)

-- newtype EnvReader a = EnvReader { runEnvReader' :: ReaderT Options (StateT ConfigNode IO) a }
--     deriving (
--         Functor, 
--         Applicative, 
--         Monad,
--         MonadIO, 
--         MonadState ConfigNode, 
--         MonadReader Options,
--         MonadThrow, 
--         MonadCatch, 
--         MonadMask)

type EnvReader o s a = ReaderT o (StateT s IO) a

runEnvReader :: MonadIO m => Options -> ConfigNode -> EnvReader Options ConfigNode a -> m ConfigNode
runEnvReader e s = liftIO . (`execStateT` s) . (`runReaderT` e)

envReaderTest :: EnvReader Options ConfigNode ()
envReaderTest = do
    opts <- ask
    let doLog = logger opts
    liftIO $ doLog "Writing a configuration node..."
    put $ ConfigNode (Just $ getName opts)
    liftIO $ doLog "Wrote configuration node..."
    throwM $ userError "what"

main :: IO ()
main = 
    let
        opts = Options {
                logger = putStrLn,
                getName = "Justin Christensen"
            }
        start = ConfigNode Nothing
    in runEnvReader opts start envReaderTest >>= liftIO . print