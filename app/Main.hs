{-# LANGUAGE OverloadedStrings         #-}

module Main where

import           Bowling.Lib

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Data.Monoid
import Waargonaut.Encode
import Waargonaut.Decode
import Waargonaut.Attoparsec
import Data.ByteString.Lazy.Char8

main = do
  let port = 3000
  Prelude.putStrLn $ "Listening on port " ++ show port
  run port app

hPlain = (hContentType, "text/plain")
hJson = (hContentType, "application/json")

app req res = do
  let method = requestMethod req
  let path   = pathInfo req
  body <- strictRequestBody req
  res $ case (requestMethod req, pathInfo req) of
    ("POST", ["scorecards"]) -> responseLBS status200 [hJson] $ simplePureEncodeByteString encodeScorecard newGame
    ("POST", ["scorecards","calculate"]) ->
      responseLBS status200 [hJson] $
        case pureDecodeAttoparsecByteString decodeScorecard $ toStrict $ body of
          Left _ -> "error"
          Right x -> case validateScorecard x of
            Left e -> pack e
            Right v -> simplePureEncodeByteString encodeScoredGame $ calculateFrames v
    _ -> responseLBS status200 [hPlain] "not implemented"
