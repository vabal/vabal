# Vabal - The Cabal Companion


 What is it?
------------

Have you ever wanted to try the new bleeding-edge GHC release, but also need older GHCs for your projects or programs (XMonad!)?

Have you ever upgraded GHC just to find all your haskell projects broken?

Have you ever dreamt about treating `base` as all other packages (i.e. change its version without much thought)?

`vabal` tries to determine a `ghc` version that complies with the `base` package constraints (and
the `Cabal` package constraints in the setup-depends section, if any) found in the `.cabal` file.
Then it uses [ghcup](https://github.com/haskell/ghcup) to fetch the compiler (if you don't have it yet)
and prints to stdout the options to pass to `cabal` to use the fetched compiler.

By default `vabal` tries to use compilers already available on the system
and downloads them only when it can't do otherwise (or fails if you use the `--no-install` flag).

This program tries to solve these issues in the easiest possible way.
The GHC compiler downloading is managed by [ghcup](https://github.com/haskell/ghcup).
No need to manually manage different ghc versions by hand!

`vabal` tries to be as little intrusive as possible, leverages `cabal`'s capabilities of working with different GHC versions,
and does *not* force you in its paradigm.
The only change to the global system state `vabal` does is to tell `ghcup` to download a compiler,
this behavior can be disabled with `--no-install` flag, so you are always in charge of what's happening!


Join our IRC channel `#vabal` on [freenode](https://webchat.freenode.net/) for discussing and getting help from the community.


 Requirements
--------------

In order to install `vabal` you need:
- `ghc` >= 8.4.1,


You can install `vabal` directly from Hackage by running:
> $ cabal v2-install vabal

(Or `cabal new-install` depending on the version of your `cabal`).

Or you can install it from sources, by running:
> $ git clone https://github.com/Franciman/vabal.git
> 
> $ cd vabal
> 
> $ cabal v2-install vabal

(Or `cabal new-install`, depending on the version of your `cabal`)

Remember to put `$HOME/.cabal/bin` in your `PATH`.


`vabal` needs to run on a `POSIX` compliant system, because it uses the `command` utility,
furthermore, these programs are required to be in `PATH`:
- ghcup,
- cabal >= 2.4.0.0


 Quick start
--------------

For starters run:
> $ vabal update

This will download updated infos about released `ghc`s and the versions of `base` they ship,
the info is stored [here](https://github.com/Franciman/vabal-ghc-metadata/blob/master/ghc-metadata.csv),
and will put them in `$HOME/.vabal/ghc-metadata.csv`.
You may want to run this from time to time, when new `ghc`s get released, so that vabal will know about them.

Then cd into your project directory and run:
> $ vabal configure

This will analyze the cabal file in the directory and extract constraints imposed on `base`,
then it will get a compatible version of `ghc` using `ghcup` (possibly downloading it)
and will finally run `cabal v2-configure` to configure the project with the obtained version of `ghc`.

If everything went fine, you now have your project configured to use a `ghc` compatible
with the constraints imposed on `base` (and the `Cabal` package constraints in the setup-depends section, if any),
you can now build your project as you're used to:
> $ cabal v2-build

You can also enable and disable flags for your package, like this:
> $ vabal configure --flags="flag1 -flagToDisable"

And if you want to pass other flags directly to `cabal v2-configure`,
you can do it after a `--`, e.g.:
> $ vabal configure -- --enable-tests --enable-shared


How to use vabal: full story
----------------------------

`vabal` tries to be composable with `cabal`, you can use it in combination with any `cabal` subcommand.
For example, if you don't want vabal to also perform the `cabal v2-configure` step,
but would like to use it in combination with `cabal v2-build`, you can run (*):
> $ vabal --flags="your -flags" -- cabal v2-build --other-cabal-options
 
`vabal` invokes the command with the options and arguments specified, *plus* additional options provided by `vabal`,
to make `cabal` the `ghc` compiler that `vabal` obtained.

> In Haskell jargon, you can see this as partially applying the `cabal` "function" first,
> and then providing it the remaining arguments that `vabal` determines.

It follows the Unix philosophy and its power comes from composition with other programs.

You can specify any command after `--`, not just `cabal`.
If you don't specify any command after `--`, then by default `echo` is executed, so it will print to stdout
the options that `vabal` would have passed to `cabal`.

In fact, `vabal configure` is just a shortcut for:
> vabal -- cabal v2-configure

In this mode you can compose `vabal` with other `cabal` commands too, also with old-style `cabal build`, `cabal configure`, etc..

There is one last command available, it is `vabal show`, it does the same things `vabal` does,
but instead of executing the provided command, it just prints the version of the obtained `ghc` to stdout.


 Gotchas
----------

Here are some known gotchas that affect `vabal`:
- `vabal` trusts the constraints imposed on `base` (and `Cabal` constraints found in the setup-depends section, if any)
that it finds in the cabal file, therefore it only finds a ghc version that makes it possible to respect the constraints,
but it is not guaranteed that the build will be successful. (Generally one should always write correct constraints).

If you want a deeper analysis that also considers other dependencies, use the `--try-hard` flag.


 Full list of options
---------------------

`vabal` and `vabal configure` accept these options:

```
  -g,--with-ghc-version VER
                           Explicitly tell which version of ghc you want to use
                           for the project. (Incompatible with option
                           --with-base-version)
  -b,--with-base-version VER
                           Specify the version of base package you want to use
                           (it must be specified fully, i.e. 4.11.0.0). It is
                           going to be checked against base constraints in the
                           cabal file for validity. (Incompatible with option
                           --with-ghc-version)
  --flags FLAGS            String containing a list of space separated flags to
                           be used to configure the project (You can enable or
                           disable a flag by adding a + or - in front of the
                           flag name. When none is specified, the flag is
                           enabled). Flag assignment determined here is
                           forwarded to cabal.
  --cabal-file FILE        Explicitly tell which cabal file to use. This option
                           is forwarded to cabal.
  --no-install             If GHC needs to be downloaded, fail, instead.
  --always-newest          Always choose newest GHC possible, don't prefer
                           already installed GHCs
  --try-hard               Try configuring the project with each compatible
                           ghcs, until one succeds. In this way the selected ghc
                           will be guaranteed to be able to solve constraints.
                           Differently from '--try-super-hard', if there are
                           multiple ghcs supporting the same base and Cabal
                           version, then only one of those is tried, so, for
                           example after ghc 8.6.3, ghc 8.6.2 is not tried, and
                           we directly try ghc 8.4.4. (Incompatible with
                           --try-super-hard)
  --try-super-hard         Try configuring the project with each compatible
                           ghcs, until one succeds. In this way the selected ghc
                           will be guaranteed to be able to solve constraints.
                           (Incompatible with --try-hard)
  -h,--help                Show help
```

with `vabal configure` you can also pass arguments directly to cabal,
just specify them after `--`, e.g.
> vabal configure -- --cabal-option --enable-tests --haddock-css=PATH


 How to uninstall it
--------------------

If you want to uninstall `vabal` you can follow the same steps
that are necessary to uninstall any executable installed with `cabal v2-install`
(i.e. remove the symlink in "$HOME/.cabal/bin").

Furthermore, you also need to remove the `$HOME/.vabal` directory
containing metadata used by `vabal`.


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

