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

  inputs = {
    systems.url = "github:nix-systems/default";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    systems,
    nixpkgs,
    treefmt-nix,
    ...
  }: let
    eachSystem = f:
      nixpkgs.lib.genAttrs (import systems) (
        system:
          f rec {
            pkgs = nixpkgs.legacyPackages.${system};
            inherit (pkgs) haskellPackages;
            epubinfo = haskellPackages.callCabal2nix "epubinfo" ./. {};
            treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
          }
      );
  in {
    packages = eachSystem ({
      pkgs,
      epubinfo,
      ...
    }: {
      epubinfo = pkgs.haskell.lib.justStaticExecutables epubinfo;
      default = self.packages.${pkgs.system}.epubinfo;
    });

    devShells = eachSystem ({
      pkgs,
      haskellPackages,
      epubinfo,
      ...
    }: {
      default = haskellPackages.shellFor {
        packages = _p: [
          epubinfo
        ];
        buildInputs = [
          haskellPackages.cabal-install
          haskellPackages.ghcid
          haskellPackages.cabal-install
          pkgs.hpack
        ];
        withHoogle = true;
      };
    });

    # Run `nix fmt [FILE_OR_DIR]...` to execute formatters configured in treefmt.nix.
    formatter = eachSystem ({
      pkgs,
      treefmtEval,
      ...
    }:
      treefmtEval.config.build.wrapper);

    checks = eachSystem ({
      pkgs,
      treefmtEval,
      ...
    }: {
      # Throws an error if any of the source files are not correctly formatted
      # when you run `nix flake check --print-build-logs`. Useful for CI
      formatting = treefmtEval.config.build.check self;
    });
  };
}
