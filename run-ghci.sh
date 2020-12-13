#!/usr/bin/env bash
set -e
# see https://gitlab.haskell.org/ghc/ghc/-/issues/10920
stack exec -- ghci -ghci-script load-all.ghci
