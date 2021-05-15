{
  description = "Extract metadata from an EPUB file";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.flake-compat.url = "github:edolstra/flake-compat";
  inputs.flake-compat.flake = false;

  outputs = { nixpkgs, flake-utils, flake-compat, ... }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
        myHaskellPackages = pkgs.haskell.packages.ghc884.override {
          overrides = hself: _hsuper: {
            "epub2json" =
              hself.callCabal2nix
                "epub2json"
                (gitignore ./.)
                { };
          };
        };
      in
      rec {
        packages.epub2json = pkgs.haskell.lib.justStaticExecutables myHaskellPackages.epub2json;
        defaultPackage = packages.epub2json;
        apps.epub2json = flake-utils.lib.mkApp {
          drv = packages.epub2json;
        };
        defaultApp = apps.epub2json;
        lib.haskellPackages = myHaskellPackages;
        lib.epub2json = lib.haskellPackages.epub2json;
        devShell = myHaskellPackages.shellFor {
          packages = p: [
            p.epub2json
          ];
          buildInputs = [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.ormolu
            pkgs.haskellPackages.hlint
            pkgs.niv
            pkgs.nixpkgs-fmt
          ];
          withHoogle = true;
          # TODO: Add shellHook for installing the pre-commit hook
          # currently defined in setup-hooks.nix
          #
          # See https://github.com/cachix/pre-commit-hooks.nix/pull/67
        };
      }
    ))
    // { inherit flake-compat; };
}
