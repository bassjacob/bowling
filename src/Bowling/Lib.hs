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

listFrames :: Scorecard -> [Frame]
listFrames (Scorecard f1 f2 f3 f4 f5 f6 f7 f8 f9 f10) = [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10]

data ScoredGame =
  ScoredGame
    { _scoredGameScore    ::  Int
    , _scoredGameComplete :: Bool
    , _scoredGameFrames   :: [(Int, Frame)]
    } deriving (Show, Eq)

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

isGameComplete :: [Frame] -> Bool
isGameComplete = foldr ((&&) . isFrameComplete) True

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

calculateFrame :: Frame -> Frame -> Frame -> (Int, Frame)
calculateFrame f1 f2 f3 =
  (addScores $ removeUnused $ [f1, f2, f3] >>= getScoresList, f1)

foo frames = foldr (\a b -> (take 3 $ drop a frames ++ [(NormalFrame Unused Unused), (NormalFrame Unused Unused)]) : b) [] [0..9]

calculateFrames :: Scorecard -> ScoredGame
calculateFrames scorecard =
  let frames = listFrames scorecard;
      permutations = foldr (\a b -> (take 3 $ drop a frames ++ [(NormalFrame Unused Unused), (NormalFrame Unused Unused)]) : b) [] [0..9];
      calculatedFrames = foldr (\a b -> (calculateFrame (a !! 0) (a !! 1) (a !! 2)) : b) [] permutations;
  in ScoredGame
    (calculateScore calculatedFrames)
    (isGameComplete frames)
    calculatedFrames

calculateScore :: [(Int, Frame)] -> Int
calculateScore = foldr ((+) . fst) 0
