{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.xdg) configHome;
  inherit (config.lib.file) mkOutOfStoreSymlink;
  l = lib // builtins;
  nvimPackage = pkgs.neovim-unwrapped;
in {
  imports = [./lunarvim.nix];
  home.shellAliases."vim" = "${nvimPackage}/bin/nvim";
  xdg.configFile."nvim".source = mkOutOfStoreSymlink "${configHome}/dotfield/home/users/${config.home.username}/config/nvim";
  home.packages = with pkgs; [
    nvimPackage
    tree-sitter

    #: lang/lsp support
    nodePackages.neovim
    python3Packages.pynvim
  ];
}
