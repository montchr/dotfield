{
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

  environment.systemPackages = [
    pkgs.foot
  ];
}
