#!/bin/sh

# First argument is directory to switch to
if [ $# -ge 1 ]; then
    cd $1
fi

# The filename to analyze
projectPath=$(pwd)
# Get first .cabal file found
cabalFilename=$(ls | grep '.cabal$' | head -n 1)

if [ -z ${cabalFilename} ]; then
    echo "No cabal file found."
    exit
fi

fullname=$(printf '%s/%s' ${projectPath} ${cabalFilename})

analyzerPath="/home/francesco/Projects/vabal/vabal-analyzer/dist-newstyle/build/x86_64-linux/ghc-8.6.2/vabal-analyzer-0.1.0.0/x/vabal-analyzer/build/vabal-analyzer/vabal-analyzer"

ghc_version=$(eval ${analyzerPath} ${cabalFilename})

printf "Suggested ghc version: %s\n" $ghc_version

# TODO: Not portable, actually. Find a way to determine these entries
arch='x86_64'
os_descr='fedora27-linux'

# Dir in which there is the compiler
outputDir="$HOME/.ghc_install_dir/bins/ghc-${ghc_version}"

# Check if we already have this version installed
if [ ! -e ${outputDir} ]; then
    echo "Installing it."
    baseUrl="https://downloads.haskell.org/~ghc/${ghc_version}/"
    buildName="ghc-${ghc_version}-${arch}-${os_descr}.tar.xz"
    downloadUrl="${baseUrl}${buildName}"

    # Make tmp dir to contain build artifacts
    tmpDir="$HOME/.ghc_install_dir/builds"
    mkdir -p ${tmpDir}
    outputFilename="${tmpDir}/ghc-${ghc_version}.tar.xz"
    curl ${downloadUrl} -o ${outputFilename}
    cd ${tmpDir}
    tar -xJf ${outputFilename}
    cd "ghc-${ghc_version}"

    mkdir -p outputDir
    ./configure --prefix=${outputDir}
    make install

    # Cleanup build artifacts
    rm -r "$HOME/.ghc_install_dir/builds"
else
    echo "Already installed."
fi

# Configure the project to use this version of ghc
echo "Configuring project."
cd ${projectPath}
cabal new-configure -w "${outputDir}/bin/ghc"

