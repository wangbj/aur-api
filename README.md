[![Build Status](https://travis-ci.org/wangbj/aur-api.svg?branch=master)](https://travis-ci.org/wangbj/aur-api)
# aur-api
implements ArchLinux AUR json API (v5) defined at:

  https://wiki.archlinux.org/index.php/AurJson

API exported:

  * info: Query metadata for list of packages (match exact names)
  * search: Synonym of ``searchBy ByNameDesc``
  * searchBy: Search a given pattern by either ``name``, ``desc``, or ``name-desc``
