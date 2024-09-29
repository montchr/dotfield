{
  lib,
  modulesPath,
  pkgs,
  ...
}:
{
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-gnome.nix"
    ./installer.nix
    ./desktop.nix
  ];

  # HACK: Override core profile
  services.openssh.settings.PermitRootLogin = lib.mkForce "yes";

  environment.systemPackages = [
    pkgs.foot
  ];
}
