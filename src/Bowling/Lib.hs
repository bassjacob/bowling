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

isFrameComplete :: Frame -> Bool
isFrameComplete (NormalFrame Pending _) = False
isFrameComplete (NormalFrame _ Pending) = False
isFrameComplete (FinalFrame Pending _ _) = False
isFrameComplete (FinalFrame _ Pending _) = False
isFrameComplete (FinalFrame _ _ Pending) = False
isFrameComplete _ = True

isGameComplete :: Scorecard -> Bool
isGameComplete (Scorecard f1 f2 f3 f4 f5 f6 f7 f8 f9 f10) =
  foldr (&&) True
    [ (isFrameComplete f1)
    , (isFrameComplete f2)
    , (isFrameComplete f3)
    , (isFrameComplete f4)
    , (isFrameComplete f5)
    , (isFrameComplete f6)
    , (isFrameComplete f7)
    , (isFrameComplete f8)
    , (isFrameComplete f9)
    , (isFrameComplete f10)
    ]
