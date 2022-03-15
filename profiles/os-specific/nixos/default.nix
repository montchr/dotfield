{
  self,
  config,
  lib,
  pkgs,
  ...
}: {
  dotfield.path = lib.mkDefault "/etc/nixos";

  nix = {
    autoOptimiseStore = true;
    optimise.automatic = true;
    systemFeatures = ["nixos-test" "benchmark" "big-parallel" "kvm"];
  };

  #shellAliases = {
  #  # Fix `nixos-option` for flake compatibility
  #  nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  #};

  networking.wireless = {
    # environmentFile = "/run/secrets/wireless.env";
    networks = {
      # bortHole.psk = "@PSK_BORTHOLE@";
    };
  };
}
