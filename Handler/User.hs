{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.User (
  getUserR, postUserR,
  ) where

import Ywitter

import Control.Monad
import Data.Maybe
import Data.Time
import Yesod.Form
import Yesod.Helpers.Auth

getUserR :: String -> Handler RepHtml
getUserR uname = do
  myuid <- requireAuthId
  mu <- runDB $ getBy (UniqueUserName uname)
  (ps, following) <- case mu of
    Just (uid, u) -> do
      ps <- runDB $ selectList [PostUserEq uid] [PostDateDesc] 20 0
  
      following <- runDB $ do
        m <- count [FollowFollowerEq myuid, FollowFolloweeEq uid]
        return $ m > 0
  
      return (zip (cycle [Just u]) ps, following)
    Nothing ->
      return ([], False)
  
  cur <- liftIO $ getCurrentTime
  let tweets = renderPosts ps cur
  let myself = isJust mu && myuid == fst (fromJust mu)
  
  defaultLayout $ do
    [hamlet|
$maybe u <- mu
  <table>
    <tr>
      <td rowspan="3" witdh="64px">
        <img src="" width="64" height="64">
      <td>#{userName $ snd u}
    <tr>
      <td>#{userLongName $ snd u}
    <tr>
      <td>#{userDesc $ snd u}

  $if myself
    それはあなたです！
  $elseif following
    <form method="post" action=@{UserR uname}>
      <input type="submit" name="unfollow" value="フォローをやめる">
  $else
    <form method="post" action=@{UserR uname}>
      <input type="submit" name="follow" value="フォローする">

  ^{tweets}
$nothing
  申し訳ありません。そのページは存在しません。
|]

postUserR :: String -> Handler ()
postUserR uname = do
  myuid <- requireAuthId
  mu <- runDB $ getBy (UniqueUserName uname)
  let Just (uid, u) = mu
  when (isNothing mu) $ redirect RedirectTemporary (UserR uname)
  cur <- liftIO $ getCurrentTime  
  
  mbf <- runFormPost' $ maybeStringInput "follow"
  case mbf of
    Just _ -> do
      runDB $ insert $ Follow myuid uid cur
      return ()
    Nothing -> do
      runDB $ deleteBy $ UniqueFollow myuid uid

  redirect RedirectTemporary (UserR uname)
