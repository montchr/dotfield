{ config, lib, pkgs, ... }:

{
  my.user.packages = with pkgs; [
    neovim-unwrapped

    # LunarVim dependencies
    cargo
  ];
}
