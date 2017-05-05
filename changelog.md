# 0.1.2

 - Add support for `Cabal-1.24` (and drop support for previous Cabal versions)
 - Add experimental support for Stack

# 0.1.1.4

 - Add support for `Cabal-1.22.*` and `base-4.8.*`

# 0.1.1.3

 - Allow `optparse-applicative-0.11.*` and `haskell-src-exts-1.16.*`

# 0.1.1.2

 - New option `--ignore-package` to white-list redundant packages by name
 - Exit with a non-zero status code if (non-ignored) redundant dependencies are found
 - Fake support for parsing `-XSafeHaskell` and `-XExplicitNamespaces` in .imports files
 - Switched from `cmdargs` to `optparse-applicative`

# 0.1.1.1

 - Minor typo in output messages fixed

# 0.1.1.0

 - Update to support `Cabal-1.18`/`Cabal-1.20`, GHC 7.8.1+, and `haskell-src-exts-1.15`

# 0.1.0.1

 - Add support for `haskell-src-exts-1.14.0`

# 0.1.0.0

 - Initial release
