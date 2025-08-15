flake@{ ... }:
{
  dotfield.hosts.nixos.installer-graphical = {
    features = with flake.config.dotfield.features; [
      graphical
      installer
    ];

    nixos =
      { modulesPath, ... }:
      {
        imports = [
          "${modulesPath}/installer/cd-dvd/installation-cd-graphical-gnome.nix"
        ];
      };
  };
}
