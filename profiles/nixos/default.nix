{ self, config, lib, pkgs, ... }:

{
  nix = {
    autoOptimiseStore = true;
    optimise.automatic = true;
    systemFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
  };

  shellAliases = {
    # Fix `nixos-option` for flake compatibility
    nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  };
}
