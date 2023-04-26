{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    quasar.url = github:queezle42/quasar;
  };

  outputs = { self, nixpkgs, quasar }:
  with nixpkgs.lib;
  let
    systems = platforms.unix;
    forAllSystems = genAttrs systems;
    getHaskellPackages = pkgs: pattern: pipe pkgs.haskell.packages [
      attrNames
      (filter (x: !isNull (strings.match pattern x)))
      (sort (x: y: x>y))
      (map (x: pkgs.haskell.packages.${x}))
      head
    ];
  in {
    packages = forAllSystems (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [
          self.overlays.default
          quasar.overlays.default
        ]; };
        haskellPackages = getHaskellPackages pkgs "ghc94.";
        results = {
          qbar = haskellPackages.qbar;
        };
      in results // {
        default = pkgs.linkFarm "qbar-all" (results // mapAttrs' (k: v: nameValuePair "${k}-doc" (v.doc or pkgs.emptyDirectory)) results);
      }
    );

    overlays.default = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
          qbar = hfinal.generateOptparseApplicativeCompletions ["qbar"]
            (hfinal.callCabal2nix "qbar" ./. {});
        };
      };
    };

    devShells = forAllSystems (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [
          self.overlays.default
          quasar.overlays.default
        ]; };
        haskellPackages = getHaskellPackages pkgs "ghc94.";
      in rec {
        default = haskellPackages.shellFor {
          packages = hpkgs: [
            hpkgs.qbar
          ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.zsh
            pkgs.entr
            pkgs.ghcid
            haskellPackages.haskell-language-server
            pkgs.hlint
          ];
        };
      }
    );
  };
}

