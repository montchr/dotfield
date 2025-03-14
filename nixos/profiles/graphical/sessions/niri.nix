{ pkgs, ... }:
{
  imports = [ ./__wlroots.nix ];

  programs.niri.enable = true;

  environment.systemPackages = with pkgs; [
    xwayland-satellite
  ];
}
