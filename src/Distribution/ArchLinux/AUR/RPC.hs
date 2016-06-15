{-# LANGUAGE OverloadedStrings #-}
module Distribution.ArchLinux.AUR.RPC
    ( info
    , search
    , searchBy
    ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Data.Text (Text)
import           Control.Monad.Except
import           Control.Monad.Catch
import           Network.Wreq
import           Control.Lens
import           Data.Aeson

import           Distribution.ArchLinux.AUR.Types

aurServer = "https://aur.archlinux.org/rpc/?v=" ++ show aurApi
  where aurApi = 5

genURL :: AURQuery -> String
genURL (QSearch Nothing txt) = aurServer ++ "&type=search&arg=" ++ txt
genURL (QSearch (Just field) txt) = aurServer ++ "&type=search&by=" ++ show field ++ "&arg=" ++ txt
genURL (QInfo q) = aurServer ++ "&type=info" ++ concatMap (\x -> "&arg%5b%5d="++x) q

queryAUR :: (MonadIO m, MonadCatch m) => AURQuery -> ExceptT String m [AURInfo]
queryAUR s = do
  r <- liftIO $ get (genURL s) `catchAll` (\(SomeException e) -> fail (show e))
  let res = eitherDecode (r ^. responseBody) >>= getAURInfo
  ExceptT (return $! res)

info :: (MonadIO m, MonadCatch m) => [String] -> ExceptT String m [AURInfo]
info = queryAUR . QInfo

search :: (MonadIO m, MonadCatch m) => String -> ExceptT String m [AURInfo]
search = queryAUR . QSearch Nothing

searchBy :: (MonadIO m, MonadCatch m) => SearchBy -> String -> ExceptT String m [AURInfo]
searchBy f = queryAUR . QSearch (Just f)

url1 = "https://aur.archlinux.org/rpc/?v=5&type=info&arg%5b%5d=icaclient"
