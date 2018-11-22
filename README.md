# Vabal - The Cabal Companion


 What is it?
------------

Have you ever wanted to try the new bleeding-edge GHC release, but also need older GHCs for your projects or programs (XMonad!)?

Have you ever upgraded GHC just to find all your haskell projects broken?

Have you ever dreamt about treating `base` as all other packages (i.e. change its version without much thought)?

`vabal` analyzes the `.cabal` file of a project and tries to determine a `ghc` version compatible with the constraints imposed on `base` 
in the `.cabal` file; then it fetches the correct `ghc` for the build and configures the project to use it.

This program tries to solve these issues in the easiest possible way.
It discovers which `base` version you need to build your package and automatically downloads the corresponding `ghc`.
The GHC compiler will be downloaded from [the official mirror](https://downloads.haskell.org/~ghc/), checked and installed for you.
No need to manually manage different ghc versions by hand!

`vabal` tries to be as little intrusive as possible, leverages `cabal`'s capabilities of working with different GHC versions,
and does *not* force you in its paradigm.
At its core `vabal` just edits `cabal.project.local`, which you can then modify as you wish;
you are always in charge of what is happening!


 How to use it
--------------

First cd into your project directory:

> $ cd my-project/

Then run:

> vabal

This will find out which GHC version is needed to build the project and then modify `cabal.project.local` to tell Cabal which compiler to use and where to find it.

That is it! You can now use `cabal` as you are accustomed to:

> cabal new-build

Now your project builds with the configured compiler and you will not get `base` version errors anymore!


 Contributors
--------------

Francesco Ariis

Francesco Magliocca

Marco Umbrello
