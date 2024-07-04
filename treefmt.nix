{
  projectRootFile = "treefmt.nix";

  # Nix
  programs.alejandra.enable = true;

  # Haskell
  programs.ormolu.enable = true;
  programs.hlint.enable = true;
}
