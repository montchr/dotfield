{
  dotfield.features."installer/graphical".nixos =
    {
      modulesPath,
      pkgs,
      ...
    }:
    {
      imports = [
        "${modulesPath}/installer/cd-dvd/installation-cd-graphical-gnome.nix"
        # FIXME: these no longer exist!
        ./installer.nix
        ./desktop.nix
      ];

      environment.systemPackages = [
        pkgs.ghostty
      ];
    };
}
