# Revision history for vabal

## 1.2.0 -- 2018-12-27

* Correctly deal with ghcup empty list of installed ghcs. Also add the ghc in PATH to the list of installed ghcs

## 1.1.0 -- 2018-12-27

* Make the vabal-internal sub-lib a public library, it plays better with hackage. Redirect ghcup output to stderr,
  because otherwise it pollutes vabal output that gets forwarded to ghcup.
 
## 1.0.0 -- 2018-12-26

* First version. Released on an unsuspecting world.
