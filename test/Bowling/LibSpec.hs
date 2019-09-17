{-# LANGUAGE OverloadedStrings         #-}

module Bowling.LibSpec
  ( spec
  )
where

import           Test.Hspec
import           Bowling.Lib
import           Waargonaut.Encode
import           Waargonaut.Decode
import           Waargonaut.Attoparsec (pureDecodeAttoparsecText)
import           Data.Text.Lazy

nonScoringGame = Scorecard
  [ NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , NormalFrame (Pindown 0) (Pindown 0)
  , FinalFrame (Pindown 0) (Pindown 0) Unused
  ]

simpleScoringGame = Scorecard
  [ NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , NormalFrame (Pindown 1) (Pindown 1)
  , FinalFrame (Pindown 1) (Pindown 1) Unused
  ]

perfectCompleteGame = Scorecard
  [ NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , NormalFrame Strike Unused
  , FinalFrame Strike Strike Strike
  ]

perfectIncompleteGame = Scorecard
  [ NormalFrame Strike  Unused
  , NormalFrame Strike  Unused
  , NormalFrame Strike  Unused
  , NormalFrame Strike  Unused
  , NormalFrame Strike  Unused
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , FinalFrame Pending Pending Pending
  ]

unplayedGame = Scorecard
  [ NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , NormalFrame Pending Pending
  , FinalFrame Pending Pending Pending
  ]

getFrames (Scorecard frames) = frames

spec :: Spec
spec = do
  describe "newGame" $ do
    it "creates a new game with all frames set to pending" $ do
      newGame `shouldBe` unplayedGame
  describe "isGameComplete" $ do
    it "marks a complete game as complete" $ do
      isGameComplete (getFrames perfectCompleteGame) `shouldBe` True
    it "marks an incomplete game as not complete" $ do
      isGameComplete (getFrames perfectIncompleteGame) `shouldBe` False
  describe "addScores" $ do
    it "correctly" $ do
      addScores [Pindown 0, Pindown 0, Pindown 1] `shouldBe` 0
      addScores [Pindown 1, Spare 9, Pindown 1, Pindown 3] `shouldBe` 11
      addScores [Strike, Strike, Strike, Pindown 3] `shouldBe` 30
      addScores [Strike, Strike, Foul, Pindown 3] `shouldBe` 20
  describe "calculateScore" $ it "correctly" $ do
    calculateScore (_scoredGameFrames $ calculateFrames perfectCompleteGame)
      `shouldBe` 300
    calculateScore (_scoredGameFrames $ calculateFrames simpleScoringGame)
      `shouldBe` 20
    calculateScore (_scoredGameFrames $ calculateFrames perfectIncompleteGame)
      `shouldBe` 120
  describe "calculateFrames" $ do
    it "correctly" $ do
      calculateFrames perfectCompleteGame `shouldBe` ScoredGame
        300
        True
        [ ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredNormalFrame 30 Strike Unused
        , ScoredFinalFrame 30 Strike Strike Strike
        ]
      calculateFrames perfectIncompleteGame `shouldBe` ScoredGame
        120
        False
        [ ScoredNormalFrame 30 Strike  Unused
        , ScoredNormalFrame 30 Strike  Unused
        , ScoredNormalFrame 30 Strike  Unused
        , ScoredNormalFrame 20 Strike  Unused
        , ScoredNormalFrame 10 Strike  Unused
        , ScoredNormalFrame 0  Pending Pending
        , ScoredNormalFrame 0  Pending Pending
        , ScoredNormalFrame 0  Pending Pending
        , ScoredNormalFrame 0  Pending Pending
        , ScoredFinalFrame 0 Pending Pending Pending
        ]
      calculateFrames unplayedGame `shouldBe` ScoredGame
        0
        False
        [ ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredNormalFrame 0 Pending Pending
        , ScoredFinalFrame 0 Pending Pending Pending
        ]
      calculateFrames simpleScoringGame `shouldBe` ScoredGame
        20
        True
        [ ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredNormalFrame 2 (Pindown 1) (Pindown 1)
        , ScoredFinalFrame 2 (Pindown 1) (Pindown 1) Unused
        ]
  describe "encodeScoredGame and decodeScoredGame" $ do
    it "correctly" $ do
      (pureDecodeAttoparsecText decodeScoredGame $ toStrict $ simplePureEncodeText encodeScoredGame $ calculateFrames perfectCompleteGame) `shouldBe` Right (calculateFrames perfectCompleteGame)
      (pureDecodeAttoparsecText decodeScoredGame $ toStrict $ simplePureEncodeText encodeScoredGame $ calculateFrames perfectIncompleteGame) `shouldBe` Right (calculateFrames perfectIncompleteGame)
      (pureDecodeAttoparsecText decodeScoredGame $ toStrict $ simplePureEncodeText encodeScoredGame $ calculateFrames nonScoringGame) `shouldBe` Right (calculateFrames nonScoringGame)
  describe "encodeScorecard and decodeScorecard" $ do
    it "correctly" $ do
      (pureDecodeAttoparsecText decodeScorecard $ toStrict $ simplePureEncodeText encodeScorecard perfectCompleteGame) `shouldBe` Right perfectCompleteGame
      (pureDecodeAttoparsecText decodeScorecard $ toStrict $ simplePureEncodeText encodeScorecard perfectIncompleteGame) `shouldBe` Right perfectIncompleteGame
      (pureDecodeAttoparsecText decodeScorecard $ toStrict $ simplePureEncodeText encodeScorecard nonScoringGame) `shouldBe` Right nonScoringGame
