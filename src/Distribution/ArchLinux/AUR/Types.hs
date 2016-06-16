-- |Primitive types used for AUR RPC (json) API.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Distribution.ArchLinux.AUR.Types
    ( SearchBy (..)
    , ReplyType (..)
    , AURQuery (..)
    , AURReply (..)
    , AURInfo (..)
    , getAURInfo
    ) where

import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Aeson.Types (Parser)
import           Data.Aeson

data SearchBy = ByName | ByNameDesc | ByMaintainer

instance Show SearchBy where
  show ByName       = "name"
  show ByNameDesc   = "name-desc"
  show ByMaintainer = "maintainer"

data AURQuery = QSearch SearchBy String
              | QInfo [String]

data ReplyType = ReplySearch | ReplyMultiInfo | ReplyError
instance Show ReplyType where
  show ReplySearch    = "search"
  show ReplyMultiInfo = "multiinfo"
  show ReplyError     = "error"

getReplyType :: String -> Parser ReplyType
getReplyType s
  | s == "search"    = return ReplySearch
  | s == "multiinfo" = return ReplyMultiInfo
  | s == "error"     = return ReplyError
  | otherwise        = fail $ "cannot parse aur return type, " ++ s ++
                              " is not one of [search, multiinfo, error] "

data AURReply f a = AURReply {
    retVersion     :: Int
  , retType        :: ReplyType
  , retResultCount :: Int
  , retResults     :: f a
  , retError       :: Maybe String
  } deriving (Show, Functor)

getAURInfo :: (Functor f,
            Applicative f,
            Foldable f,
            Traversable f) =>
              AURReply f a ->
              Either String (f a)
getAURInfo (AURReply _ t n r e) = case t of
  ReplySearch    -> return r
  ReplyMultiInfo -> return r
  ReplyError     -> maybe (Left "") Left e
  
instance
  (Functor f,
   Applicative f,
   Foldable f,
   Traversable f,
   FromJSON a,
   FromJSON (f a)) => FromJSON (AURReply f a) where
  parseJSON (Object v) = AURReply
                    <$>  v .:  "version"
                    <*> (v .:  "type" >>= getReplyType)
                    <*>  v .:  "resultcount"
                    <*>  v .:  "results"
                    <*>  v .:? "error"
 
data AURInfo = AURInfo {
    packageID             :: Int
  , packageName           :: Text
  , packagePackageBaseID  :: Int
  , packagePackageBase    :: Text
  , packageVersion        :: Text
  , packageDescription    :: Maybe Text
  , packageURL            :: Maybe Text
  , packageNumVotes       :: Int
  , packagePopularity     :: Double
  , packageOutOfDate      :: Maybe Int
  , packageMaintainer     :: Maybe Text
  , packageFirstSubmitted :: Int
  , packageLastModified   :: Int
  , packageURLPath        :: Text
  , packageDepends        :: [Text]
  , packageMakeDepends    :: [Text]
  , packageOptDepends     :: [Text]
  , packageConflicts      :: [Text]
  , packageLicense        :: [Text]
  , packageKeywords       :: [Text]
  } deriving (Show)

instance FromJSON AURInfo where
  parseJSON (Object v) = AURInfo
                    <$> v .:  "ID"
                    <*> v .:  "Name"
                    <*> v .:  "PackageBaseID"
                    <*> v .:  "PackageBase"
                    <*> v .:  "Version"
                    <*> v .:? "Description"
                    <*> v .:? "URL"
                    <*> v .:  "NumVotes"
                    <*> v .:  "Popularity"
                    <*> v .:  "OutOfDate"
                    <*> v .:? "Maintainer"
                    <*> v .:  "FirstSubmitted"
                    <*> v .:  "LastModified"
                    <*> v .:  "URLPath"
                    <*> v .:! "Depends"     .!= []
                    <*> v .:! "MakeDepends" .!= []
                    <*> v .:! "OptDepends"  .!= []
                    <*> v .:! "Conflicts"   .!= []
                    <*> v .:! "License"     .!= []
                    <*> v .:! "Keywords"    .!= []
