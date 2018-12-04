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


 How to use it
--------------

There are two requirements to use `vabal`, you need both `cabal` and `ghcup` in your `$PATH`.


First cd into your project directory:

> $ cd my-project/

Then run:

> vabal

This will find out which GHC version is needed to build the project and then updates `cabal.project.local` to tell `cabal` which compiler to use and where to find it.

That is it! You can now use `cabal` as you are accustomed to:

> cabal new-build

Now your project builds with the configured compiler and you will not get `base` version errors anymore!


 Contributors
--------------

Francesco Ariis

Francesco Gazzetta

Francesco Magliocca

Marco Umbrello
