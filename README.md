# bahug

[![Build Status](https://travis-ci.org/githubuser/bahug.png)](https://travis-ci.org/githubuser/bahug)

To run:

    stack build && stack exec bahug

To see splices:

    stack clean && stack build --ghc-options=-ddump-splices
    find . -name "*.dump-splices" -exec cat {} \;
