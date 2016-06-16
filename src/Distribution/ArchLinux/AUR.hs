-- | Implements AUR json API (v5)
--
-- AUR json API spec can be found at https://wiki.archlinux.org/index.php/AurJson
--
module Distribution.ArchLinux.AUR
   ( SearchBy (..)
   , AURInfo (..)
   , info
   , search
   , searchBy
   ) where

import Distribution.ArchLinux.AUR.Types
import Distribution.ArchLinux.AUR.RPC
