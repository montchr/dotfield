let
  nixos-builder-vm = import ./darwinModules/nixos-builder-vm/default.nix;
in {inherit nixos-builder-vm;}
