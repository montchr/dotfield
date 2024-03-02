{
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  home.shellAliases."d" = "direnv";
}
