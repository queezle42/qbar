{
  outputs = { self, nixpkgs }: let

    lib = nixpkgs.lib;

    systems = lib.platforms.unix;

    forAllSystems = f: lib.genAttrs systems (system: f system);

  in {

    packages = forAllSystems (system: {
      qbar = import ./. {
        pkgs = nixpkgs.legacyPackages."${system}";
      };
    });

    overlay = self: super: {
      qbar = self.haskellPackages.qd;
      haskell = super.haskell // {
        packageOverrides = hself: hsuper: super.haskell.packageOverrides hself hsuper // {
          qbar = import ./. { pkgs = self; haskellPackages = hself; };
        };
      };
    };

    defaultPackage = forAllSystems (system: self.packages.${system}.qbar);

    devShell = forAllSystems (system: self.packages.${system}.qbar.env);

  };
}
