{ profiles, suites, pkgs, ... }:
{
  imports = [
    ./configuration.nix
  ] ++ suites.base
    ++ suites.identity
    ++ suites.hardware;

  bud.enable = true;
  bud.localFlakeClone = "/etc/nixos";

  environment.systemPackages = with pkgs; [
    libnotify
  ];
}
