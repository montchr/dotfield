{ config, lib, pkgs, ... }:

{
  home.sessionVariables = {
    LUNARVIM_RUNTIME_DIR = "${config.xdg.dataHome}/lunarvim";
    LUNARVIM_CONFIG_DIR = "${config.xdg.configHome}/lvim";
    LUNARVIM_CACHE_DIR = "${config.xdg.cacheHome}/nvim";
  };

  home.packages = with pkgs; [
    neovim-unwrapped

    ##: LunarVim dependencies {{

    #: core
    cargo
    fd
    ripgrep

    #: nodejs
    nodePackages.neovim
    tree-sitter

    #: python
    python39Packages.pynvim

    #: }}
  ];
}
