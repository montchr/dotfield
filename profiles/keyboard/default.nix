{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # https://ergodox-ez.com/pages/wally-planck
    wally-cli
  ];
}
