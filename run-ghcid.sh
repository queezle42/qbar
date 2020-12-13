#!/usr/bin/env bash
set -e
stack exec -- ghcid -c "ghci -ghci-script load-all.ghci"
