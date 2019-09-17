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
  describe "isGameComplete" $ do
    it "marks a complete game as complete" $ do
      isGameComplete (listFrames perfectCompleteGame) `shouldBe` True
    it "marks an incomplete game as not complete" $ do
      isGameComplete (listFrames perfectIncompleteGame) `shouldBe` False
  describe "addScores" $ do
    it "correctly" $ do
      addScores [Pindown 0, Pindown 0, Pindown 1] `shouldBe` 0
      addScores [Pindown 1, Spare 9, Pindown 1, Pindown 3] `shouldBe` 11
      addScores [Strike, Strike, Strike, Pindown 3] `shouldBe` 30
      addScores [Strike, Strike, Foul, Pindown 3] `shouldBe` 20
  describe "calculateScore" $ do
    it "correctly" $ do
      calculateScore (_scoredGameFrames $ calculateFrames perfectCompleteGame) `shouldBe` 300
      calculateScore (_scoredGameFrames $ calculateFrames simpleScoringGame) `shouldBe` 20
      calculateScore (_scoredGameFrames $ calculateFrames perfectIncompleteGame) `shouldBe` 120
  describe "calculateFrames" $ do
    it "correctly" $ do
      calculateFrames perfectCompleteGame `shouldBe` ScoredGame 300 True (zip (replicate 10 30) (listFrames perfectCompleteGame))
      calculateFrames perfectIncompleteGame `shouldBe` ScoredGame 120 False (zip ((replicate 3 30) ++ [20, 10] ++ (replicate 5 0)) (listFrames perfectIncompleteGame))
      calculateFrames unplayedGame `shouldBe` ScoredGame 0 False (zip (replicate 10 0) (listFrames unplayedGame))
      calculateFrames simpleScoringGame `shouldBe` ScoredGame 20 True (zip (replicate 10 2) (listFrames simpleScoringGame))
