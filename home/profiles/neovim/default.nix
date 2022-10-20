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
  home.shellAliases."vim" = l.getExe nvimPackage;
  xdg.configFile."nvim" = mkOutOfStoreSymlink "${configHome}/nvim";
  home.packages = with pkgs; [
    nvimPackage
    tree-sitter

    #: lang/lsp support
    nodePackages.neovim
    python3Packages.pynvim
  ];
}
