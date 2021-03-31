{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages, args ? {}}:

let
  inherit (pkgs) lib haskell;

  rawdrv = haskellPackages.callCabal2nix "qbar" ./. args;
  drv = haskell.lib.generateOptparseApplicativeCompletions [ "qbar" ] rawdrv;

in

  if lib.inNixShell then drv.env else drv
