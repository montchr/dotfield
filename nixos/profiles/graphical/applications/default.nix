{ pkgs, ... }:
{
  imports = [
    ./firefox.nix
    ./zoom-us.nix
  ];

  environment.systemPackages = [
    pkgs.signal-desktop
  ];
}
