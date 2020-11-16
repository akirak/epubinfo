{ compiler ? "ghc884" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: _hsuper: {
      "epub2json" =
        hself.callCabal2nix
          "epub2json"
          (gitignore ./.)
          { };
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."epub2json"
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
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."epub2json");

  docker = pkgs.dockerTools.buildImage {
    name = "epub2json";
    config.Cmd = [ "${exe}/bin/epub2json" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "epub2json" = myHaskellPackages."epub2json";
}
