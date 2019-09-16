{-# LANGUAGE OverloadedStrings         #-}

module Main where

import Lib

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

main = do
  let port = 3000
  Prelude.putStrLn $ "Listening on port " ++ show port
  run port app

plain = (hContentType, "text/plain")
json = (hContentType, "application/json")

app req res = do
  let method = requestMethod req
  let path = pathInfo req
  body <- strictRequestBody req
  res $
    case (requestMethod req, pathInfo req) of
      ("POST", ["scorecards"]) -> responseLBS status200 [plain] undefined
      ("POST", ["scorecards/calculate"]) -> responseLBS status200 [plain] undefined
