{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "JSON Schema to Elm converter";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=5df4d78d54f7a34e9ea1f84a22b4fd9baebc68d0";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });
    in
    {
      overlay = (final: prev: {
        json-schema-to-elm = final.haskellPackages.callCabal2nix "json-schema-to-elm" ./. { };
      });
      packages = forAllSystems (system: {
        json-schema-to-elm = nixpkgsFor.${system}.json-schema-to-elm;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.json-schema-to-elm);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.json-schema-to-elm ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            haskellPackages.ormolu
            cabal-install
            haskellPackages.hlint
            haskellPackages.apply-refact
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
    };
}
