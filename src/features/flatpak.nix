{ inputs, ... }:
{
  aspects.graphical.nixos = {
    imports = [ inputs.nix-flatpak.nixosModules.nix-flatpak ];

    services.flatpak.enable = true;
  };

  aspects.graphical.home = {
    imports = [ inputs.nix-flatpak.homeManagerModules.nix-flatpak ];
  };
}
