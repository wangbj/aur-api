module Main where

import Control.Monad.Except

import Distribution.ArchLinux.AUR

main :: IO ()
main = runExceptT (search "libtinfo") >>= print
