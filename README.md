# pkgdbgc

garbage collection for haskell new-style global databases ("store")

## Typical Basic Usage

1. Register local dist-newstyle directories

    ~~~~
    find $HOME -name dist-newstyle -exec pkgdbgc register-dist-newstyle {} \;
    ~~~~

2. Run the gc that deletes all packages not reachable from those directories
   (i.e. the current build configs in dist-newstyle dirs provide the roots
   of the gc).
   
   ~~~~.sh
   pkgdbgc store-gc
   # or using specified ghc-pkg, to affect different ghc version package-db:
   pkgdbgc --with-ghc-pkg=path/to/my/ghc-pkg store-gc
   # Also consider the --dry-run --verbose flags to see what would be deleted
   ~~~~

## Detailed Usage

~~~~
NAME

  pkgdbgc - garbage collection for ghc-pkg databases

USAGE

  pkgdbgc [--compiler COMPILER] [--store-path STOREPATH] [--with-ghc-pkg GHCPKG]
          [-h,--help] [--version] [--dryrun,--dry-run] [-v,--verbose]
          [<command>]

DESCRIPTION

  The intended usage of this program is to garbage-collect (gc) the cabal "store" package database
  used with cabal's new-build feature. The roots for the garbage collection are tracked by looking
  at the `plan.json` file in dist-newstyle directories. `plan.json` might not be the only sensible
  source for roots (even manual registration might make sense in certain cases) but for now there is
  no explicit support for any other sources.
  
  This program uses a (user-)global registry (placed in XDGCONFIGDIR/pkgdbgc/rootregistry.yaml) in
  order to track two things:
  
    1) a set of "root" packages
    2) a set of dist-newstyle build directories
  
  roots determine the packages to retain during gc and the dist-newstyle directories are saved in
  order to update the roots before performing gc.
  
  See `help <subcommand>` for the commands listed below.
  
  There is NO WARRANTY, to the extent permitted by law.
  This program is free software released under the BSD3 license.
  See https://github.com/lspitzner/pkgdbgc
  Please report bugs at https://github.com/lspitzner/pkgdbgc/issues

COMMANDS

  help                print help about this command
  register-dist-newstyle
                      add dist-newstyle dir to global registry
  store-gc            perform gc on a cabal store

ARGUMENTS

  --compiler COMPILER determines which of the package-dbs in the store to affect. Will by default be
                      derived from the ghc-pkg version.
  --with-ghc-pkg GHCPKG
                      path of ghc-pkg command to be used; this will also determine which package-db
                      in the store will be affected.
~~~~

## Building

- Currently requires HEAD of the butcher repository.

## Known Issues

- Flags and build settings are currently not tracked. This means for example
  that if you build with profiling, then without profiling, and then run the
  gc, the profiling versions of dependencies are gc'ed.
