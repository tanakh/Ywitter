{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.User (
  getUserR,
  ) where

import Ywitter

getUserR :: String -> Handler RepHtml
getUserR uname = do
  mu <- runDB $ getBy (UniqueUserName uname)
  ps <-
    case mu of
      Just (uid, _) -> do
        runDB $ selectList [PostUserEq uid] [PostDateDesc] 20 0
      Nothing ->
        return []
  defaultLayout $ do
    [hamlet|
$maybe u <- mu
  TODO
$nothing
  申し訳ありません。そのページは存在しません。
|]
