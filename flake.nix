{
  description = "Extract metadata from an EPUB file";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }: (flake-utils.lib.eachDefaultSystem (
    system: let

      pkgs = nixpkgs.legacyPackages.${system};

      inherit (pkgs) haskellPackages;

      epubinfo = haskellPackages.callCabal2nix "epubinfo" ./. {};
    in rec {
      packages = {
        epubinfo = pkgs.haskell.lib.justStaticExecutables epubinfo;
        default = self.packages.${system}.epubinfo;
      };
      devShells = {
        default = haskellPackages.shellFor {
          packages = _p: [
            epubinfo
          ];
          buildInputs = with haskellPackages; [
            cabal-install
            ghcid
            cabal-install
          ];
          withHoogle = true;
        };

        # A minimal devShell without extra buildInputs for printing the GHC
        # version on CI.
        ghc = pkgs.mkShell {
          buildInputs = [
            haskellPackages.ghc
          ];
        };
      };
    }
  ));
}
