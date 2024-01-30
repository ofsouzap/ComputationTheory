module Main where

import Test.Hspec
  ( hspec )

import qualified RegisterMachineTests ( spec )

main :: IO ()
main = hspec $ do
  RegisterMachineTests.spec
