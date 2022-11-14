{
  config,
  lib,
  pkgs,
  ...
}: let
  l = lib // builtins;
in {
  imports = [
    ./custom.nix
    # ./lunarvim.nix
  ];

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withNodeJs = true;
    withPython3 = true;
    withRuby = true;
  };
}
