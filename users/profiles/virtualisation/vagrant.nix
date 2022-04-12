{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    ruby
    rbenv
    rubocop
  ];
}
