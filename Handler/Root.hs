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
        fs <- selectList [FollowFollowerEq uid] [] 0 0
        ps <- selectList [PostUserIn $ uid : map (followFollowee . snd) fs] [PostDateDesc] 20 0
        us <- forM ps $ \(_, p) -> get $ postUser p
        return $ zip us ps
      Nothing -> do
        return []
    
    cur <- liftIO $ getCurrentTime
    let tweets = renderPosts posts cur

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
