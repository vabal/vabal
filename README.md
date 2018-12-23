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


 Quick start
--------------

For starters
> vabal update

This will download updated infos about released `ghc`s and the versions of `base` they ship,
the info is stored [here](https://github.com/Franciman/vabal-ghc-metadata/blob/master/ghc-metadata.csv).
You may want to run this from time to time, when new `ghc`s get released, so that vabal will know about them.

`vabal` follows the `UNIX` philosophy, its power comes when it gets combined with other commands.

Running inside your project directory:

> vabal

will make `vabal` find out which GHC versions is needed to build the project and obtain it,
then it will print to stdout the options to pass to `cabal`to make it use the obtained GHC compiler.

The intended way to use vabal is to compose it with `cabal` through the utility `xargs` (*), like this:

> vabal | xargs -r cabal new-build

This command will run vabal and then `xargs` will read from stdin the arguments vabal provided
and appends them after cabal new-build, executing the resulting command.
The `-r` flag is needed when vabal fails, in that case xargs won't execute the cabal command.
You only need to use it if you're on `Linux`, on `OS X` there is no need for it.
(See more about this in the (*) Remark).

Here's an example:

> vabal | xargs -r cabal new-configure

is equivalent to running:

> cabal new-configure -w /path/to/obtained/ghc


If you specify some flags to vabal, they will be sent over to cabal as well:

> vabal --flags='a -b' | xargs -r cabal new-configure

is equivalent to:

> cabal new-configure -w /path/to/ghc --flags='a -b'

In this way you can combine vabal with all cabal subcommands (new-build, new-configure, new-test, etc...).

As an example, suppose you want to configure your project using the `ghc` version vabal chooses for you,
you just need to:

> cd project-dir/
> vabal | xargs -r cabal new-configure

That is it! Now your project with build with the configured compiler and you will not get `base` version errors anymore!

Most of the time you may just want to run

> vabal | xargs -r cabal new-configure

so it may be convenient to add an alias or write a custom script.

(*) Remark: 
> The `-r` flag you see is only available in the GNU version of `xargs`. It makes xargs fail if its input is empty,
> i.e. when vabal fails. This is necessary or cabal will run as well without arguments from vabal.
> If you use the BSD version of `xargs`, then this flag is not necessary (and is neither available),
> because this is the default behavior.
> Another possibility is to use the `-p` flag for `xargs` which is available on all `POSIX` systems,
> it will show the command it is about to run and ask the user if he needs to proceed. In this way,
> if vabal failed, he can stop the execution.
> 
> Consult your system's manpage for `xargs` for further infos.


 Gotchas
----------

Here are some known gotchas that affect `vabal`:
- `vabal` trusts the constraints imposed on `base` that it finds in the cabal file,
therefore it only finds a ghc version that makes it possible to respect the constraints,
but it is not guaranteed that the build will be successful. (Generally one should always write correct constraints)

- The current way `vabal` is meant to be used is a bit verbose and not really beginner friendly,
probably we should also add a shortcut script `vabal-configure` since it is the most common case of using vabal.


 Program usage
---------------

```
vabal - The Cabal Companion

Usage: vabal ([COMMAND] | ([-g|--with-ghc-version VER] |
             [-b|--with-base-version VER]) [--flags FLAGS] [--cabal-file FILE]
             [--no-install] [--always-newest])
  Find out a version of the GHC compiler that satisfies the constraints imposed
  on base in the cabal project (By default already installed GHCs are
  preferred). Then print to stdout the options to pass to cabal in order to use
  that compiler (potentially downloading it).

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

Available commands:
  update                   Download updated ghc metadata.
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

