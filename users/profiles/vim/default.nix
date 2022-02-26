{ config, lib, pkgs, ... }:

{
  my.env = {
    LUNARVIM_RUNTIME_DIR = "$XDG_DATA_HOME/lunarvim";
    LUNARVIM_CONFIG_DIR = "$XDG_CONFIG_HOME/lvim";
    LUNARVIM_CACHE_DIR = "$XDG_CACHE_HOME/nvim";
  };

  my.user.packages = with pkgs; [
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
