{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nodejs-16_x
    (yarn.override { nodejs = nodejs-16_x; })
  ];
}
