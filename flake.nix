{
  outputs = { self, nixpkgs }: with nixpkgs.lib; let

    systems = platforms.unix;
    forAllSystems = f: genAttrs systems (system: f system);

  in {

    packages = forAllSystems (system:
      let pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
      in rec {
        default = qbar;
        qbar = pkgs.haskellPackages.qbar;
      }
    );

    defaultPackage = forAllSystems (system: self.packages.${system}.qbar);

    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
          qbar = import ./. {
            pkgs = final;
            haskellPackages = hfinal;
          };
        };
      };
    };

    devShell = forAllSystems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in pkgs.mkShell {
        inputsFrom = [ self.packages.${system}.default.env ];
        packages = [
          pkgs.cabal-install
          pkgs.zsh
          pkgs.entr
          pkgs.ghcid
          pkgs.haskell-language-server
        ];
      }
    );

  };
}
