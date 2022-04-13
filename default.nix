{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, args ? {} }:

let
  rawdrv = haskellPackages.callCabal2nix "qbar" ./. args;
  drv = pkgs.haskell.lib.generateOptparseApplicativeCompletions [ "qbar" ] rawdrv;

in
  if pkgs.lib.inNixShell then rawdrv.env else drv
