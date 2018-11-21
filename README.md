Vabal - The Cabal Companion
---

# What is it?
This program analyzes the cabal file of a project and finds the base version needed to build it.
Then it tries to get the necessary ghc version to build the project and configures the project to use it.


# Rationale
Have you ever upgraded ghc and broke all your haskell projects?
Have you ever wanted to try the new bleeding edge GHC release, but you also need older GHC for your projects or programs (XMonad!)?


Have you ever dreamt about treating the base package as all other packages? I.e. change its version without much thought


This program tries to solve these issues in the easiest way possible.
It discovers which base version you need to build your package and automatically downloads the corresponding GHC compiler.
You don't need to manually manage different ghc versions by hand.

Vabal tries to be the least intrusive possible, it tries to leverage all cabal's capabilities of working with different ghc versions,
and also doesn't force you in its paradigm.
At its core vabal just edits the cabal.project.local file, which you can then remove or edit as you wish,
so you're still in charge of what's happening


# How to use it


First cd into your project directory
> $ cd my-project/


Then run
> vabal


This will try to figure out which ghc version is needed to build the project and writes the cabal.project.local to tell cabal which ghc compiler to use


Finally just keep using cabal as you are accustomed to.


This will build your project using the configured compiler and you won't get base version errors anymore!
> cabal new-build
