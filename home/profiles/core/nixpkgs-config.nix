moduleArgs @ {
  self,
  inputs,
  lib,
  ...
}: let
  inherit
    (inputs)
    agenix
    emacs-overlay
    gitignore
    nix-dram
    nixpkgs-wayland
    nur
    ;
in
  lib.mkIf (!(moduleArgs.osConfig.home-manager.useGlobalPkgs or false)) {
    nixpkgs = {
      config.allowUnfree = true;
      # https://github.com/nix-community/home-manager/issues/2942
      config.allowUnfreePredicate = pkg: true;
      overlays = [
        agenix.overlay
        emacs-overlay.overlay
        gitignore.overlay
        # nix-dram.overlay
        nixpkgs-wayland.overlay
        # nur.overlay

        # self.overlays.internalLib
        self.overlays.packages
        self.overlays.overrides
      ];
    };
  }
