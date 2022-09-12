{
  self,
  inputs,
  ...
}: let
  inherit
    (inputs)
    agenix
    gitignore
    nix-dram
    nixpkgs-wayland
    nur
    ;
in {
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      agenix.overlay
      gitignore.overlay
      nix-dram.overlay
      nixpkgs-wayland.overlay
      nur.overlay

      # self.overlays.internalLib
      # self.overlays.packages
      self.overlays.overrides
    ];
  };
}
