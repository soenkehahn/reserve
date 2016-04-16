{-# LANGUAGE QuasiQuotes #-}

module InterpreterSpec where

import           Prelude.Compat

import           Control.Monad.IO.Class
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Test.Hspec
import           Test.Mockery.Directory

import           Interpreter

spec :: Spec
spec = do
  describe "start" $ do
    it "allows to pass command line arguments" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ unindent [i|
          import System.Environment
          main = getArgs >>= writeFile "args" . show
        |]
        withInterpreter "Main.hs" $ do
          start ["foo"]
        readFile "args" `shouldReturn` show ["foo"]

  describe "reload" $ do
    it "returns compile errors as Lefts" $ do
      inTempDirectory $ do
        writeFile "Main.hs" $ unindent [i|
          main = foo
        |]
        withInterpreter "Main.hs" $ do
          result <- reload
          liftIO $ result `shouldBe`
            Left "Main.hs:1:8: Not in scope: ‘foo’"
