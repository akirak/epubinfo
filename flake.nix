{
  description = "Extract metadata from an EPUB file";

  nixConfig = {
    extra-substituters = [
      "https://akirak.cachix.org"
    ];
    extra-trusted-public-keys = [
      "akirak.cachix.org-1:WJrEMdV1dYyALkOdp/kAECVZ6nAODY5URN05ITFHC+M="
    ];
  };

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
      };
    }
  ));
}
