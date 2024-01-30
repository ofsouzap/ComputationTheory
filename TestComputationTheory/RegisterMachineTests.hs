{-# LANGUAGE ScopedTypeVariables #-}
module RegisterMachineTests where

import Test.Hspec
  ( Spec
  , describe
  , it )
import Test.QuickCheck
  ( property )

spec :: Spec
spec =
  describe "Register Machine" $ do
    it "sanity check" $ property $
      \ (x :: Bool) -> x == x
