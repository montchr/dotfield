{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ agenix rage ];
}
