-- | Implements AUR json API (v5)
--
-- AUR json API spec can be found at https://wiki.archlinux.org/index.php/AurJson
--
-- * 'info': Get metadata for list of packages (match exact names)
--
-- * 'searchBy': Search a given pattern by either /name/, /desc/, or /name-desc/
--
-- * 'search': Synonym of /searchBy ByNameDesc/.
--
module Distribution.ArchLinux.AUR
   ( SearchBy (..)
   , AURInfo (..)
   , info
   , searchBy
   , search
   ) where

import Distribution.ArchLinux.AUR.Types
import Distribution.ArchLinux.AUR.RPC
