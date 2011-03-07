{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Setting (
  getSettingR,
  postSettingR
  ) where

import Control.Applicative
import Data.Maybe

import Ywitter

data SettingParams = SettingParams
  { uname :: String
  }

settingParamsFormlet :: Maybe SettingParams -> Form s m SettingParams
settingParamsFormlet mparams = fieldsToTable $ SettingParams
  <$> stringField "ユーザ名" (fmap uname mparams)

getSettingR :: Handler RepHtml
getSettingR = do
  (uix, u) <- requireAuth
  let name = userName u
  let isDefaultName = head name == '*'
  
  let s = SettingParams (if isDefaultName then "" else name)
  (res, form, enctype) <- runFormGet $ settingParamsFormlet $ Just s
  
  defaultLayout $ do
    setTitle "設定"
    [hamlet|
$if isDefaultName
  <p .info>
    まずはユーザ名を設定してください。

<form method="post" action=@{SettingR} enctype=#{enctype}>
  <table>
    ^{form}
    <tr>
      <td colspan"2">
        <input type="submit" value="設定">
|]

postSettingR :: Handler ()
postSettingR = do
  (uix, _) <- requireAuth
  res <- runFormPost' $ settingParamsFormlet Nothing
  
  runDB $ update uix $ [UserName $ uname res]
  
  redirect RedirectTemporary SettingR
