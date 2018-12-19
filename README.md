# Vabal - The Cabal Companion


 What is it?
------------

Have you ever wanted to try the new bleeding-edge GHC release, but also need older GHCs for your projects or programs (XMonad!)?

Have you ever upgraded GHC just to find all your haskell projects broken?

Have you ever dreamt about treating `base` as all other packages (i.e. change its version without much thought)?

`vabal` tries to determine a `ghc` version that complies with the `base` package constraints found in the `.cabal` file.
Then it uses [ghcup](https://github.com/haskell/ghcup) to fetch the compiler (if you don't have it yet)
and prints to stdout the path to the fetched compiler.

By default `vabal` tries to use compilers already available on the system
and downloads them only when it can't do otherwise (or fails if you use the `--no-install` flag).

This program tries to solve these issues in the easiest possible way.
The GHC compiler downloading is managed by [ghcup](https://github.com/haskell/ghcup).
No need to manually manage different ghc versions by hand!

`vabal` tries to be as little intrusive as possible, leverages `cabal`'s capabilities of working with different GHC versions,
and does *not* force you in its paradigm.
The only change to the global system state `vabal` does is to tell `ghcup` to download a compiler,
this behavior can be disabled with `--no-install` flag, so you are always in charge of what's happening!


 Requirements
--------------

These programs are required to be in `PATH`:
- ghcup


 Quick start
--------------

Running inside your project directory:

> vabal

will make `vabal` find out which GHC versions is needed to build the project and obtain it,
then it will print to stdout the path to the obtained GHC compiler.

This can be combined with `cabal`'s -w options.
So to build the project using the detected compiler, you can run:

> cabal new-build -w "$(vabal)"

If you want to use the chosen compiler persistently,
you can just configure the project to use it, like this:

> cabal new-configure -w "$(vabal)"

That is it! Now your project with build with the configured compiler and you will not get `base` version errors anymore!

NB: 
> The `$(vabal)` syntax is command substitution in `sh` and `bash` shells, it will replace `$(vabal)` with `vabal`'s output
> (for example `cabal new-build -w "$(vabal)"` becomes `cabal new-build -w /path/to/nice/ghc/version/ghc`)
>
> If you are using `fish` shell command substitution is done like this: `cabal new-build (vabal)`.
>
> Consult your shell's manual to see how command substitution is done.


 Gotchas
----------

Here are some known gotchas that affect `vabal`:
- vabal trusts the constraints imposed on `base` that it finds in the cabal file,
therefore it only finds a ghc version that makes it possible to respect the constraints,
but it is not guaranteed that the build will be successful. (Generally one should always write correct constraints)

- if you want to specify some flags to be used when configuring a package, right now you should repeat them twice,
once to pass them to cabal and once for vabal: `cabal new-configure -fmyflag -w "$(vabal --flags='myflag')"`.
The same thing goes for the `--cabal-file` flag.


 Program usage
---------------

```
vabal - The Cabal Companion

Usage: vabal ([-g|--with-ghc-version VER] | [-b|--with-base-version VER])
             [--flags FLAGS] [--cabal-file FILE] [--no-install]
             [--always-newest]
  Find out a version of the GHC compiler that satisfies the constraints imposed
  on base in the cabal project (By default already installed GHCs are
  preferred). Then print to stdout the path to a GHC compiler with that version
  (potentially downloading it).

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
  --always-newest          Always choose newest GHC possible, don't prefer
                           already installed GHCs
  -h,--help                Show this help text
```


 Contributing
--------------

Pull Requests and suggestions are welcome.


 Issues
--------

You can signal issues [here](https://github.com/Franciman/vabal/issues)


 Contributors
--------------

Francesco Ariis

Francesco Gazzetta

Francesco Magliocca

Lorenzo Tabacchini

Marco Umbrello

