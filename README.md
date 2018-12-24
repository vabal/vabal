# Vabal - The Cabal Companion


 What is it?
------------

Have you ever wanted to try the new bleeding-edge GHC release, but also need older GHCs for your projects or programs (XMonad!)?

Have you ever upgraded GHC just to find all your haskell projects broken?

Have you ever dreamt about treating `base` as all other packages (i.e. change its version without much thought)?

`vabal` tries to determine a `ghc` version that complies with the `base` package constraints found in the `.cabal` file.
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


 Requirements
--------------

These programs are required to be in `PATH`:
- ghcup
- cabal >= 2.4.1.0


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
and will finally run `cabal new-configure` to configure the project with the found version of `ghc`.

If everything went fine, you now have your project configured to use a `ghc` compatible
with the constraints imposed on `base`, you can now build your project as you're used to:
> $ cabal new-build

You can also enable and disable flags for your package, like this:
> $ vabal configure --flags="flag1 -flagToDisable"

And if you want to pass other flags directly to `cabal new-configure`,
you can do it after a `--`, e.g.:
> $ vabal configure -- --enable-tests --enable-shared


How to use vabal: full story
----------------------------

`vabal` tries to leverage the power of composability of shell commands,
you can use it in combination with any `cabal` subcommand.
For example, if you don't want vabal to also perform the `cabal new-configure` step,
but would like to use it in combination with `cabal new-build`, you can run (*):
> $ vabal --flags="your -flags" | xargs -r cabal new-build

(If you are on `OS X`, you don't need to specify the -r option for xargs)

xargs invokes cabal with the arguments and options specified *plus* options read from stdin.
Read the *Remark* for info about the `-r` option.

> You can see this as partially applying the `cabal` "function" first, and then providing it the remaining arguments that `vabal` emitted.

What vabal actually does is analyze the cabal file, obtain a suitable `ghc` and then print to stdout
options to pass to cabal (already properly escaped to be used with xargs).
It follows the Unix philosophy and its power comes from composition with other programs.

In fact, `vabal configure` is just a shortcut for:
> vabal | xargs -r cabal new-configure

(*) Remark: 
> The `-r` flag you see is only available in the GNU version of `xargs`. It makes xargs fail if its input is empty,
> i.e. when vabal fails. This is necessary or cabal will run as well without arguments from vabal.
> If you use the BSD version of `xargs`, then this flag is not necessary (and is neither available),
> because this is the default behavior.
> Another possibility is to use the `-p` flag for `xargs` which is available on all `POSIX` systems,
> it will show the command it is about to run and ask the user if he wants to proceed. In this way,
> if vabal failed, he can stop the execution.
> 
> Consult your system's manpage for `xargs` for further info.


 Gotchas
----------

Here are some known gotchas that affect `vabal`:
- `vabal` trusts the constraints imposed on `base` that it finds in the cabal file,
therefore it only finds a ghc version that makes it possible to respect the constraints,
but it is not guaranteed that the build will be successful. (Generally one should always write correct constraints)


 Full list of options
---------------------

`vabal` and `vabal configure` accept these options:

```
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
                           enabled). Flag assignment determined here is also
                           emitted to stdout as a cabal option
                           (or passed to "cabal new-configure" in the case of
                           "vabal configure")
                           
  --cabal-file FILE        Explicitly tell which cabal file to use. This option
                           also emitted to stdout as cabal option (or passed to
                           `cabal new-configure` in the case of "vabal configure")
  
  --no-install             If GHC needs to be downloaded, fail, instead.
  
  --always-newest          Always choose newest GHC possible, don't prefer
                           already installed GHCs
```

with `vabal configure` you can also pass arguments directly to cabal,
just specify them after `--`, e.g.
> vabal configure -- --cabal-option --enable-tests --haddock-css=PATH


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

