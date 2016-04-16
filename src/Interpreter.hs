{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Interpreter (
  InterpreterM
, withInterpreter
, start
, stop
, reload
) where

import           Prelude.Compat

import           Control.Concurrent
import           Control.Exception (throwIO, AsyncException(UserInterrupt))
import           Control.Monad.Catch (catch)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Language.Haskell.Interpreter hiding (get)
import           System.Environment
import           System.Exit

type InterpreterM
  = InterpreterT (StateT (Maybe ThreadId) (ReaderT FilePath IO))

withInterpreter :: FilePath -> InterpreterM a -> IO a
withInterpreter src action = do
  r <- runReaderT (evalStateT (runInterpreter (reload >> action)) Nothing) src
  case r of
    Right a -> return a
    Left err -> die $ show err

start :: [String] -> InterpreterM ()
start args = do
  action <- interpret "Main.main" (return () :: IO ())
  thread <- liftIO $ forkIO $
    withArgs args action
  lift $ put $ Just thread

stop :: InterpreterM ()
stop = do
  mThread <- lift get
  forM_ mThread $ \ thread ->
    liftIO $ throwTo thread UserInterrupt
  lift $ put Nothing

reload :: InterpreterM (Either String ())
reload = try $ do
  src <- lift ask
  loadModules [src]
  loaded <- getLoadedModules
  setTopLevelModules (loaded)

try :: InterpreterM () -> InterpreterM (Either String ())
try action = catch (action >> return (Right ())) $ \ case
  WontCompile errs -> return $ case nub $ map errMsg errs of
    messages -> Left $ intercalate "\n" messages
  otherError -> liftIO $ throwIO $ otherError
