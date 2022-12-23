{
  description = "Extract metadata from an EPUB file";

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
            "epubinfo" =
              hself.callCabal2nix
                "epubinfo"
                (gitignore ./.)
                { };
          };
        };
      in
      rec {
        packages.epubinfo = pkgs.haskell.lib.justStaticExecutables myHaskellPackages.epubinfo;
        defaultPackage = packages.epubinfo;
        apps.epubinfo = flake-utils.lib.mkApp {
          drv = packages.epubinfo;
        };
        defaultApp = apps.epubinfo;
        lib.haskellPackages = myHaskellPackages;
        lib.epubinfo = lib.haskellPackages.epubinfo;
        devShell = myHaskellPackages.shellFor {
          packages = p: [
            p.epubinfo
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
