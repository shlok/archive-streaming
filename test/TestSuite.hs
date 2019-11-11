--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import qualified Codec.Archive.Streaming.Tests
import Test.Tasty (TestTree, defaultMain, testGroup)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "Tests"
        [ testGroup "Codec.Archive.Streaming.Tests"
                     Codec.Archive.Streaming.Tests.tests ]

--------------------------------------------------------------------------------
