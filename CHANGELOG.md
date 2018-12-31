# Revision history for vabal

## 2.0.0 -- 2019-01-01

* Add the vabal show command.
* Now vabal prints the path to ghc even when using the ghc in PATH,
  so that xargs won't be confused by empty input even when vabal hasn't failed.
* When deciding which ghc version to use, the Cabal constraints
  found in the setup-depends section of the .cabal file are taken into account, too.

## 1.2.0 -- 2018-12-27

* Correctly deal with ghcup empty list of installed ghcs.
* Add the ghc in PATH to the list of installed ghcs

## 1.1.0 -- 2018-12-27

* Make the vabal-internal sub-lib a public library, it plays better with hackage.
* Redirect ghcup output to stderr, because otherwise it pollutes vabal output that gets forwarded to ghcup.
 
## 1.0.0 -- 2018-12-26

* First version. Released on an unsuspecting world.
