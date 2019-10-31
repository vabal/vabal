#!/bin/perl

use strict;
use diagnostics;
use warnings;

my $version = shift;

my $directory="fake-ghc/$version";

# CREATE STACK FILE
open STACK, ">", "stack.yaml";

my $stack_yaml=<<"ENDSTACK";
resolver: ghc-$version
packages:
- .
ENDSTACK

print STACK $stack_yaml;
close STACK;

#INIT ENV
system "stack setup";

# Create compiler directory
mkdir $directory;

# GHC script generation

# Get info output
my $info = `stack exec -- ghc --info`;

# Get supported features
my $supported_langs = `stack exec -- ghc --supported-languages`;


my $ghc_script_vars =<<"ENDSCRIPT";
#!/bin/sh
numeric_version="$version"
version="The Glorious Glasgow Haskell Compilation System, version $version"
info='$info'
supported_languages='$supported_langs'


ENDSCRIPT

my $ghc_script_body=<<'ENDSCRIPT';
case "$@" in
"--numeric-version") echo "${numeric_version}";;
"--version") echo "${version}";;
"--info") echo "${info}";;
"--supported-languages") echo "${supported_languages}";;
esac

unset numeric_version version info supported_languages
ENDSCRIPT

my $ghc_script = $ghc_script_vars . $ghc_script_body;

open GHC, ">", "$directory/ghc";
print GHC $ghc_script;
close GHC;

# GHC-PKG script generation

# Get package database
my $package_db = `stack exec -- ghc-pkg dump --global -v0`;

my $ghc_pkg_script_vars=<<"ENDSCRIPT";
#!/bin/sh
version="$version"
pkg_db='$package_db'


ENDSCRIPT

my $ghc_pkg_script_body=<<'ENDSCRIPT';
case "$@" in
"--version") echo "GHC package manager version ${version}";;
"dump --global -v0") echo "${pkg_db}";;
esac

unset version pkg_db
ENDSCRIPT

my $ghc_pkg_script = $ghc_pkg_script_vars . $ghc_pkg_script_body;

open GHC_PKG, ">", "$directory/ghc-pkg";
print GHC_PKG $ghc_pkg_script;
close GHC_PKG;
