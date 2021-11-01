{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ruby
    rbenv
    rubocop
  ];
}
