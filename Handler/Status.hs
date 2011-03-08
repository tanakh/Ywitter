{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Status (
  getStatusR,
  ) where

import Ywitter

import Control.Applicative
import Data.Maybe

getStatusR :: String -> Integer -> Handler RepHtml
getStatusR user postid = do
  mm <- runDB $ do
    mu <- getBy (UniqueUserName user)
    mp <- get (fromIntegral postid)
    return $ (,) <$> mu <*> mp
  
  let Just (u, p) = mm
  let notFound = isNothing mm
    
  defaultLayout $ do
    [hamlet|
$if notFound
  申し訳ありません。そのページは存在しません。
$else
  <table>
    <tr>
      <td rowspan=3>
        <img src="" width="64" height="64">
      <td>#{postContent p}
|]

