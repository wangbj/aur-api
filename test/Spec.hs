{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Aeson
import           Data.Either
import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.Formatters
import           Distribution.ArchLinux.AUR
import           Distribution.ArchLinux.AUR.Types

aur1 :: LBS.ByteString
aur1="{\"version\":5,\"type\":\"search\",\"resultcount\":1,\"results\":[{\"ID\":257824,\"Name\":\"icaclient\",\"PackageBaseID\":54694,\"PackageBase\":\"icaclient\",\"Version\":\"13.3-0\",\"Description\":\"Citrix Receiver for x86_64 (64bit) Linux (ICAClient)\",\"URL\":\"http:\\/\\/www.citrix.com\\/English\\/ps2\\/products\\/product.asp?contentID=1689163&ntref=prod_top\",\"NumVotes\":65,\"Popularity\":3.069167,\"OutOfDate\":null,\"Maintainer\":\"fordprefect\",\"FirstSubmitted\":1323370174,\"LastModified\":1450528326,\"URLPath\":\"\\/cgit\\/aur.git\\/snapshot\\/icaclient.tar.gz\"}]}"

hspec_search1 =
  it "returned search result can be decoded" $
    isRight ( (eitherDecode aur1 >>= getAURInfo) :: Either String ([] AURInfo))

err1 :: LBS.ByteString
err1 = "{\"version\":5,\"type\":\"error\",\"resultcount\":0,\"results\":[],\"error\":\"Incorrect by field specified.\"}"

hspec_err1 =
  it "Check if error type is can be decoded" $
    isRight (eitherDecode err1 :: Either String (AURReply [] AURInfo) )

hspec_err2 =
  it "Check if error type is trapped by a Left value" $
    isLeft ( (eitherDecode err1 >>= getAURInfo) :: Either String ([] AURInfo))

multi1 :: LBS.ByteString
multi1 =  "{\"version\":5,\"type\":\"multiinfo\",\"resultcount\":1,\"results\":[{\"ID\":229417,\"Name\":\"cower\",\"PackageBaseID\":44921,\"PackageBase\":\"cower\",\"Version\":\"14-2\",\"Description\":\"A simple AUR agent with a pretentious name\",\"URL\":\"http:\\/\\/github.com\\/falconindy\\/cower\",\"NumVotes\":590,\"Popularity\":24.595536,\"OutOfDate\":null,\"Maintainer\":\"falconindy\",\"FirstSubmitted\":1293676237,\"LastModified\":1441804093,\"URLPath\":\"\\/cgit\\/aur.git\\/snapshot\\/cower.tar.gz\",\"Depends\":[\"curl\",\"openssl\",\"pacman\",\"yajl\"],\"MakeDepends\":[\"perl\"],\"License\":[\"MIT\"],\"Keywords\":[]}]}"

hspec_multi1 =
  it "returned search result can be decoded" $
    isRight ( (eitherDecode multi1 >>= getAURInfo) :: Either String ([] AURInfo))

spec :: Spec
spec = do
  describe "Search result" hspec_search1
  describe "Error result"  hspec_err1
  describe "Error result"  hspec_err2
  describe "Multi result"  hspec_multi1

specMain :: IO ()
specMain = hspecWith defaultConfig { configFormatter = Just progress } spec

main :: IO ()
main = specMain
