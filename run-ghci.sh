#!/usr/bin/env bash
set -e
stack exec -- ghci -ghci-script load-all.ghci
