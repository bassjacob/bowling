{-# LANGUAGE OverloadedStrings         #-}

module Bowling.LibSpec (spec) where

import Test.Hspec
import Bowling.Lib

nonScoringGame =
  Scorecard
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (NormalFrame  (Pindown 0) (Pindown 0))
    (FinalFrame  (Pindown 0) (Pindown 0) Unused)

simpleScoringGame =
  Scorecard
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (NormalFrame  (Pindown 1) (Pindown 1))
    (FinalFrame  (Pindown 1) (Pindown 1) Unused)

perfectCompleteGame =
  Scorecard
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (FinalFrame   Strike Strike Strike)

perfectIncompleteGame =
  Scorecard
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Strike Unused)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (FinalFrame   Pending Pending Pending)

unplayedGame =
  Scorecard
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (NormalFrame  Pending Pending)
    (FinalFrame   Pending Pending Pending)

spec :: Spec
spec = do
  describe "newGame" $ do
    it "creates a new game with all frames set to pending" $ do
      newGame `shouldBe` unplayedGame
