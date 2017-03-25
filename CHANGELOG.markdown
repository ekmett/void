0.7.2
-----
* Only depend on `deepseq`, `hashable`, and `semigroups` if using GHC 7.8 or earlier.
* Cleaned up spurious "redundant constraint" warnings on GHC 8+

0.7.1
-----
* Support `semigroups` 0.17 on older GHCs
* Backported `NFData`'s `semigroup` instance to older GHCs.

0.7
---
* adapt to `Data.Void` being moved into `base-4.8`
* `vacuousM` removed

0.6
---
* `instance Exception Void`
* `instance Generic Void`
* `instance Hashable Void`

0.5.12
------
* Fixed compatibility with GHC 7.2 (#6)
* Added `CHANGELOG.markdown` and `README.markdown`
