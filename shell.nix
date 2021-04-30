{ pkgs ? import <nixpkgs> { } }:
let
  sources = import ./nix/sources.nix;
  gitignore = import sources."gitignore.nix" { inherit (pkgs) lib; };
  pre-commit = (import sources."pre-commit-hooks.nix").run {
    src = gitignore.gitignoreSource ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      nix-linter.enable = true;
      ormolu.enable = true;
      hlint.enable = true;
      hpack.enable = true;
    };
  };
in
pkgs.mkShell {
  buildInputs = [
    (import ./compat.nix).shellNix
  ];

  inherit (pre-commit) shellHook;
}
