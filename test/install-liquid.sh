#!/bin/sh
if [ ! -f $HOME/.local/bin/liquid ]; then
    cd $HOME
    git clone --recursive https://github.com/ucsd-progsys/liquidhaskell.git
    cd liquidhaskell
    stack install
    cd $TRAVIS_BUILD_DIR
fi
