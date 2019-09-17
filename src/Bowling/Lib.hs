{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE LambdaCase #-}

module Bowling.Lib where

import           Waargonaut.Encode              ( Encoder
                                                , Encoder'
                                                )
import qualified Waargonaut.Encode             as E
import           Waargonaut.Decode              ( Decoder )
import qualified Waargonaut.Decode             as D

data Score = Pindown Int | Spare Int | Strike | Foul | Pending | Unused deriving (Show, Eq)

data Frame = NormalFrame Score Score | FinalFrame Score Score Score deriving (Show, Eq)

newtype Scorecard = Scorecard [Frame] deriving (Show, Eq)
data ScoredFrame = ScoredNormalFrame Int Score Score | ScoredFinalFrame Int Score Score Score deriving (Show, Eq)

data ScoredGame =
  ScoredGame
    { _scoredGameScore    ::  Int
    , _scoredGameComplete :: Bool
    , _scoredGameFrames   :: [ScoredFrame]
    } deriving (Show, Eq)

newGame = Scorecard
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
getValue (Spare   i) = i
getValue Strike      = 10
getValue Foul        = 0
getValue Pending     = 0
getValue Unused      = 0

combine (0, t) _ = (0, t)
combine (n, t) a = (n - 1, getValue a + t)

addScores :: [Score] -> Int
addScores (Strike : x       : y : _) = 10 + getValue x + getValue y
addScores (_      : Spare _ : z : _) = 10 + getValue z
addScores (x                : y : _) = getValue x + getValue y

removeUnused :: [Score] -> [Score]
removeUnused = filter (/= Unused)

getScoresList (NormalFrame s1 s2  ) = [s1, s2]
getScoresList (FinalFrame s1 s2 s3) = [s1, s2, s3]

calculateFrame :: Frame -> Frame -> Frame -> ScoredFrame
calculateFrame f1 f2 f3 =
  let score = addScores $ removeUnused $ [f1, f2, f3] >>= getScoresList
  in  case f1 of
        NormalFrame s1 s2   -> ScoredNormalFrame score s1 s2
        FinalFrame s1 s2 s3 -> ScoredFinalFrame score s1 s2 s3

calculateFrames :: Scorecard -> ScoredGame
calculateFrames (Scorecard frames) =
  let permutations = map
        (\a ->
          take 3
            $  drop a frames
            ++ [NormalFrame Unused Unused, NormalFrame Unused Unused]
        )
        [0 .. 9]
      calculatedFrames =
          map (\a -> calculateFrame (head a) (a !! 1) (a !! 2)) permutations
  in  ScoredGame (calculateScore calculatedFrames)
                 (isGameComplete frames)
                 calculatedFrames

calculateScore :: [ScoredFrame] -> Int
calculateScore = foldr add 0
 where
  add (ScoredNormalFrame score _ _ ) x = score + x
  add (ScoredFinalFrame score _ _ _) x = score + x

encodeScore :: Applicative f => Encoder f Score
encodeScore = E.mapLikeObj $ \case
  Pindown x -> E.textAt "type" "pindown" . E.intAt "pins" x
  Spare   x -> E.textAt "type" "spare" . E.intAt "pins" x
  Strike    -> E.textAt "type" "strike" . E.intAt "pins" 10
  Foul      -> E.textAt "type" "foul" . E.intAt "pins" 0
  Pending   -> E.textAt "type" "pending" . E.intAt "pins" 0
  Unused    -> E.textAt "type" "unused" . E.intAt "pins" 0

decodeScore :: Monad f => Decoder f Score
decodeScore = D.withCursor $ \c -> do
  o     <- D.down c
  _type <- D.fromKey "type" D.text o
  pins  <- D.fromKey "pins" D.int o
  pure $ case _type of
    "pindown" -> Pindown pins
    "spare"   -> Spare pins
    "strike"  -> Strike
    "foul"    -> Foul
    "pending" -> Pending
    "unused"  -> Unused

decodeFrame :: Monad f => Decoder f Frame
decodeFrame = D.withCursor $ \c -> do
  o     <- D.down c
  roll1 <- D.fromKey "roll 1" decodeScore o
  roll2 <- D.fromKey "roll 2" decodeScore o
  roll3 <- D.fromKeyOptional "roll 3" decodeScore o
  pure $ case roll3 of
    Nothing  -> NormalFrame roll1 roll2
    (Just r) -> FinalFrame roll1 roll2 r
decodeScorecard :: Monad f => Decoder f Scorecard
decodeScorecard = D.withCursor $ \c -> do
  o      <- D.down c
  frames <- D.fromKey "frames" (D.list decodeFrame) o
  pure $ Scorecard frames

encodeFrame :: Applicative f => Encoder f Frame
encodeFrame = E.mapLikeObj $ \case
  NormalFrame s1 s2 ->
    E.atKey' "roll 1" encodeScore s1 . E.atKey' "roll 2" encodeScore s2
  FinalFrame s1 s2 s3 ->
    E.atKey' "roll 1" encodeScore s1
      . E.atKey' "roll 2" encodeScore s2
      . E.atKey' "roll 3" encodeScore s3

encodeScorecard :: Applicative f => Encoder f Scorecard
encodeScorecard = E.mapLikeObj
  $ \(Scorecard frames) -> E.atKey' "frames" (E.list encodeFrame) frames

decodeScoredFrame :: Monad f => Decoder f ScoredFrame
decodeScoredFrame = D.withCursor $ \c -> do
  o     <- D.down c
  total <- D.fromKey "total" D.int o
  roll1 <- D.fromKey "roll 1" decodeScore o
  roll2 <- D.fromKey "roll 2" decodeScore o
  roll3 <- D.fromKeyOptional "roll 3" decodeScore o
  pure $ case roll3 of
    Nothing  -> ScoredNormalFrame total roll1 roll2
    (Just r) -> ScoredFinalFrame total roll1 roll2 r
decodeScoredGame :: Monad f => Decoder f ScoredGame
decodeScoredGame = D.withCursor $ \c -> do
  o      <- D.down c
  complete <- D.fromKey "complete" D.bool o
  total <- D.fromKey "total" D.int o
  frames <- D.fromKey "frames" (D.list decodeScoredFrame) o
  pure $ ScoredGame total complete frames

encodeScoredFrame :: Applicative f => Encoder f ScoredFrame
encodeScoredFrame = E.mapLikeObj $ \case
  ScoredNormalFrame total s1 s2 ->
    E.atKey' "total" E.int total
      . E.atKey' "roll 1" encodeScore s1
      . E.atKey' "roll 2" encodeScore s2
  ScoredFinalFrame total s1 s2 s3 ->
    E.atKey' "total" E.int total
      . E.atKey' "roll 1" encodeScore s1
      . E.atKey' "roll 2" encodeScore s2
      . E.atKey' "roll 3" encodeScore s3

encodeScoredGame :: Applicative f => Encoder f ScoredGame
encodeScoredGame = E.mapLikeObj $ \game ->
  E.boolAt "complete" (_scoredGameComplete game)
    . E.intAt "total" (_scoredGameScore game)
    . E.atKey' "frames" (E.list encodeScoredFrame) (_scoredGameFrames game)

validateScorecard :: Scorecard -> Either String Scorecard
validateScorecard (Scorecard frames) = do
  _ <- validateLength
  _ <- validateFinalFrame
  pure (Scorecard frames)
    where
      validateLength = if length frames == 10 then Right () else Left "Must have 10 Frames in game"
      validateFinalFrame = case last frames of
        FinalFrame _ Pending Pending -> Right ()
        FinalFrame Strike _ _ -> Right ()
        FinalFrame (Pindown x) (Spare y) _ -> Right ()
        FinalFrame (Pindown x) (Pindown y) Unused -> Right ()
        FinalFrame _ _ (Pindown _) -> Left "Can only take three rolls in final frame on a strike or a spare"
        FinalFrame _ _ Foul -> Left "Can only take three rolls in final frame on a strike or a spare"
        _ -> Left "Last frame in game must be a FinalFrame"
