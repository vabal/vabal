# Vabal - The Cabal Companion


 What is it?
------------

Have you ever wanted to try the new bleeding-edge GHC release, but also need older GHCs for your projects or programs (XMonad!)?

Have you ever upgraded GHC just to find all your haskell projects broken?

Have you ever dreamt about treating `base` as all other packages (i.e. change its version without much thought)?

`vabal` tries to determine a `ghc` version that complies with the `base` package constraints found in the `.cabal` file.
Then it uses [ghcup](https://github.com/haskell/ghcup) to fetch the compiler (if you don't have it yet) and configures the project to use it.

This program tries to solve these issues in the easiest possible way.
The GHC compiler downloading is managed by [ghcup](https://github.com/haskell/ghcup).
No need to manually manage different ghc versions by hand!

`vabal` tries to be as little intrusive as possible, leverages `cabal`'s capabilities of working with different GHC versions,
and does *not* force you in its paradigm.
At its core `vabal` just edits `cabal.project.local`, which you can then modify as you wish;
you are always in charge of what is happening!


 Requirements
--------------

These programs are required to be in `PATH`:
- cabal-install >= 2.4
- ghcup


 How to use it
--------------

First cd into your project directory:

> $ cd my-project/

Then run:

> vabal

This will find out which GHC version is needed to build the project and then runs `cabal new-configure`
telling it which compiler to use and where to find it.

That is it! You can now use `cabal` as you are accustomed to:

> cabal new-build

Now your project builds with the configured compiler and you will not get `base` version errors anymore!


 Program usage
---------------

```
vabal - The Cabal Companion

Usage: vabal ([-g|--with-ghc-version VER] | [-b|--with-base-version VER])
             [--flags FLAGS] [--cabal-file FILE] [--no-install]
  Find out a version of the GHC compiler that satisfies the constraints imposed
  on base in the cabal project. then configure the cabal project to use this
  version of the compiler.

Available options:
  -g,--with-ghc-version VER
                           Explicitly tell which version of ghc you want to use
                           for the project. (Incompatible with option
                           --with-base-version)
  -b,--with-base-version VER
                           Specify the version of base package you want to use.
                           It is going to be checked against base constraints in
                           the cabal file for validity. (Incompatible with
                           option --with-ghc-version)
  --flags FLAGS            String containing a list of space separated flags to
                           be used to configure the project (You can enable or
                           disable a flag by adding a + or - in front of the
                           flag name. When none is specified, the flag is
                           enabled).
  --cabal-file FILE        Explicitly tell which cabal file to use.
  --no-install             If GHC needs to be downloaded, fail, instead.
  -h,--help                Show this help text
```


 Contributors
--------------

Francesco Ariis

Francesco Gazzetta

Francesco Magliocca

Lorenzo Tabacchini

Marco Umbrello
