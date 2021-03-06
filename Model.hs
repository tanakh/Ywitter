{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Model where

import Yesod
import Database.Persist.TH (share2)
import Database.Persist.GenericSql (mkMigrate)
import Data.Time
import Data.Text

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share2 mkPersist (mkMigrate "migrateAll") [persist|
User
    ident String
    name String Update
    password String Maybe Update
    longName String Update default=""
    desc Text Update default=""
    UniqueUser ident
    UniqueUserName name
Email
    email String
    user UserId Maybe Update
    verkey String Maybe Update
    UniqueEmail email
Post
    user UserId Eq In
    content Text
    replyTo PostId Maybe
    replyToUser UserId Maybe
    date UTCTime Desc
Follow
    follower UserId Eq
    followee UserId Eq
    date UTCTime Desc
    UniqueFollow follower followee
|]
