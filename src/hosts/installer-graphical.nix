flake@{ ... }:
{
  dotfield.hosts.nixos.installer-graphical = {
    aspects = with flake.config.dotfield.aspects; [
      graphical
      installer
    ];

    nixos =
      { modulesPath, ... }:
      {
        imports = [
          "${modulesPath}/installer/cd-dvd/installation-cd-graphical-gnome.nix"
        ];

        # FIXME: handle this in a way that allows for building an iso
        # for aarch64-linux also
        nixpkgs.hostPlatform = "x86_64-linux";
      };
  };
}
