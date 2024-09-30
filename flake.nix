{
  description = "Extract metadata from an EPUB file";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  nixConfig = {
    extra-substituters = [
      "https://akirak.cachix.org"
    ];
    extra-trusted-public-keys = [
      "akirak.cachix.org-1:WJrEMdV1dYyALkOdp/kAECVZ6nAODY5URN05ITFHC+M="
    ];
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      systems = import inputs.systems;
      perSystem =
        {
          system,
          ...
        }:
        {
          haskellProjects.default = {
            packages = { };
            settings = { };
          };

          packages.default = self.packages.${system}.epubinfo;

          treefmt = {
            flakeCheck = true;
            projectRootFile = "package.yaml";
            programs = {
              actionlint.enable = true;
              ormolu.enable = true;
              cabal-fmt.enable = true;
              nixfmt.enable = true;
              biome.enable = true;
              # Disable hlint for now as I don't want to fix the warnings for
              # now
              #
              # hlint.enable = true;
            };
          };
        };
    };
}
