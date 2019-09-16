module Bowling.Lib where

data Score = Pindown Int | Spare Int | Strike | Foul | Pending | Unused deriving (Show, Eq)

data Frame = NormalFrame Score Score | FinalFrame Score Score Score deriving (Show, Eq)

data Scorecard =
  Scorecard
    Frame
    Frame
    Frame
    Frame
    Frame
    Frame
    Frame
    Frame
    Frame
    Frame
  deriving (Show, Eq)

newGame =
  Scorecard
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (NormalFrame Pending Pending)
    (FinalFrame  Pending Pending Pending)
