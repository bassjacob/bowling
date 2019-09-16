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

getValue :: Score -> Int
getValue (Pindown i) = i
getValue (Spare i) = i
getValue Strike = 10
getValue Foul = 0
getValue Pending = 0
getValue Unused = 0

combine (0, t) _ = (0, t)
combine (n, t) a = (n - 1, (getValue a) + t)

addScores :: [Score] -> Int
addScores (Strike:x:y:_) = 10 + getValue x + getValue y
addScores (_:(Spare _):z:_) = 10 + getValue z
addScores (x:y:_) = getValue x + getValue y

removeUnused :: [Score] -> [Score]
removeUnused = filter (\s -> s /= Unused)

getScoresList (NormalFrame s1 s2) = [s1, s2]
getScoresList (FinalFrame s1 s2 s3) = [s1, s2, s3]

calculateFrame :: Frame -> Frame -> Frame -> Int
calculateFrame f1 f2 f3 =
  addScores $ removeUnused $ [f1, f2, f3] >>= getScoresList


calculateFrames :: Scorecard -> [Int]
calculateFrames (Scorecard f1 f2 f3 f4 f5 f6 f7 f8 f9 f10) =
  [ (calculateFrame f1 f2 f3)
  , (calculateFrame f2 f3 f4)
  , (calculateFrame f3 f4 f5)
  , (calculateFrame f4 f5 f6)
  , (calculateFrame f5 f6 f7)
  , (calculateFrame f6 f7 f8)
  , (calculateFrame f7 f8 f9)
  , (calculateFrame f8 f9 f10)
  , (calculateFrame f9 f10 (NormalFrame Unused Unused))
  , (calculateFrame f10 (NormalFrame Unused Unused) (NormalFrame Unused Unused))
  ]

calculateScore :: Scorecard -> Int
calculateScore scorecard = foldr (+) 0 $ calculateFrames scorecard
