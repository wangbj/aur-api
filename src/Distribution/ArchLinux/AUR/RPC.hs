-- | AUR json API implementation (v5)
--
-- AUR json API spec can be found at https://wiki.archlinux.org/index.php/AurJson
--
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
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Data.Aeson
import           Distribution.ArchLinux.AUR.Types

aurServer = "https://aur.archlinux.org/rpc/?v=" ++ show aurApi
  where aurApi = 5

genURL :: AURQuery -> String
genURL (QSearch Nothing txt) = aurServer ++ "&type=search&arg=" ++ txt
genURL (QSearch (Just field) txt) = aurServer ++ "&type=search&by=" ++ show field ++ "&arg=" ++ txt
genURL (QInfo q) = aurServer ++ "&type=info" ++ concatMap (\x -> "&arg%5b%5d="++x) q

get s = join $ liftM2 httpLbs (parseUrl (genURL s)) (newManager tlsManagerSettings)

getM :: (MonadIO m) => AURQuery -> ExceptT String m (Response LBS.ByteString)
getM s = ExceptT . liftIO $ fmap Right (get s) `catch` (\(SomeException e) -> return (Left (show e)))

queryAUR :: (MonadIO m) => AURQuery -> ExceptT String m [AURInfo]
queryAUR s = getM s >>= \r -> ExceptT . return $ (eitherDecode >=> getAURInfo) (responseBody r)

-- |Query info of given list of packages, match exact names
-- possible return types are /multiinfo/ and /error/.
-- /error/ type is captured by ExceptT (Left).
-- However, query may return empty list which isn't considered as an error.
info :: (MonadIO m) => [String] -> ExceptT String m [AURInfo]
info = queryAUR . QInfo

-- |search given string on AUR server
-- possible return types are /search/ and /error/.
-- Like 'info', /error/ is captured by a Left.
search :: (MonadIO m) => String -> ExceptT String m [AURInfo]
search = queryAUR . QSearch Nothing

-- |searchBy field 'SearchBy' given string on AUR server
-- possible return types are /search/ and /error/.
-- Like 'search', /error/ is captured by a Left.
searchBy :: (MonadIO m) => SearchBy -> String -> ExceptT String m [AURInfo]
searchBy f = queryAUR . QSearch (Just f)

url1 = "https://aur.archlinux.org/rpc/?v=5&type=info&arg%5b%5d=icaclient"
