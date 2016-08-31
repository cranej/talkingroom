{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Backend
    (getPost
    ) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Time.Clock         (UTCTime)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Post
    slug String
    title String
    content String
    createdDate UTCTime
    tags [String]
    UniqueSlug slug
    deriving Show
Tweet
    content String
    createdDate UTCTime
    deriving Show
|]

getPost :: String -> IO Post
getPost t = undefined

getPostBySlug :: String -> IO Post
getPostBySlug slug = undefined

