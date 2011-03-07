{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root (
  getRootR,
  postRootR,
  ) where

import Ywitter

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Time

getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    case mu of
      Just (uid, u) -> do
        case userName u of
          '*':_ -> redirect RedirectTemporary SettingR
          _ -> return ()
      _ -> return ()
    
    posts <- case mu of
      Just (uid, u) -> runDB $ do
        ps <- selectList [PostUserEq uid] [PostDateDesc] 20 0
        us <- forM ps $ \(_, p) -> get $ postUser p
        return $ zip us ps
      Nothing -> do
        return []
    
    cur <- liftIO $ getCurrentTime

    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "ywitter homepage"
        addWidget $(widgetFile "homepage")

postRootR :: Handler ()
postRootR = do
  (uid, u) <- requireAuth
  mcontent <- runFormPost' $ maybeStringInput "content"
  when (isNothing mcontent || fromJust mcontent == "" || length (fromJust mcontent) > 140) $ do
    redirect RedirectTemporary RootR
  
  let Just content = mcontent
  cur <- liftIO $ getCurrentTime
  
  runDB $ do
    insert $ Post uid (T.pack content) Nothing Nothing cur
  
  redirect RedirectTemporary RootR

formatDiffTime cur prev
  | diff < 1 = "現在"
  | diff < 60 = show (round diff) ++ "秒前"
  | diff < 60*60 = show (round $ diff / 60) ++ "分前"
  | diff < 60*60*24 = show (round $ diff / 60 / 60) ++ "時間前"
  | otherwise = show prev
  where
    diff = cur `diffUTCTime` prev
