-- | AUR json API implementation (v5)
--
-- AUR json API spec can be found at https://wiki.archlinux.org/index.php/AurJson
--
{-# LANGUAGE OverloadedStrings #-}
module Distribution.ArchLinux.AUR.RPC
    ( info
    , searchBy
    , search
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
genURL (QSearch field txt) = aurServer ++ "&type=search&by=" ++ show field ++ "&arg=" ++ txt
genURL (QInfo q) = aurServer ++ "&type=info" ++ concatMap (\x -> "&arg%5b%5d="++x) q

get s = join $ liftM2 httpLbs (parseUrl (genURL s)) (newManager tlsManagerSettings)

getM :: (MonadIO m) => AURQuery -> ExceptT String m (Response LBS.ByteString)
getM s = ExceptT . liftIO $ fmap Right (get s) `catch` (\(SomeException e) -> return (Left (show e)))

queryAUR :: (MonadIO m) => AURQuery -> ExceptT String m [AURInfo]
queryAUR s = getM s >>= \r -> ExceptT . return $ (eitherDecode >=> getAURInfo) (responseBody r)

concatMapM :: (Monad m, Foldable t) => (a -> m [b]) -> t a -> m [b]
concatMapM op = foldr f (return [])
    where
      f x xs = do x <- op x
                  if null x then xs else fmap (x ++) xs

-- |Query info of given list of packages, match exact names
-- possible return types are /multiinfo/ and /error/.
-- /error/ type is captured by ExceptT (Left).
-- However, query may return empty list which isn't considered as an error.
info :: (MonadIO m) => [String] -> ExceptT String m [AURInfo]
info = concatMapM (queryAUR . QInfo) . chunks

-- Avoid encode arbitrary long URL when ``info`` package list is too long.
chunks :: [String] -> [[String]]
chunks s = if null s then [] else s1 : chunks s'
  where (s1, s') = splitAt 1024 s

-- |searchBy field 'SearchBy' given string on AUR server
-- possible return types are /search/ and /error/.
-- Like 'info', /error/ is captured by a Left.
searchBy :: (MonadIO m) => SearchBy -> String -> ExceptT String m [AURInfo]
searchBy f = queryAUR . QSearch f

-- |synonym of 'searchBy' /ByNameDesc/
search :: (MonadIO m) => String -> ExceptT String m [AURInfo]
search = searchBy ByNameDesc

url1 = "https://aur.archlinux.org/rpc/?v=5&type=info&arg%5b%5d=icaclient"
