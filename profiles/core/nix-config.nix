{
  config,
  lib,
  pkgs,
  ...
}: let
  substituters = [
    "https://cache.nixos.org/"
    "https://dotfield.cachix.org"
    "https://nix-community.cachix.org"
    "https://nixpkgs-wayland.cachix.org"
  ];
  trusted-substituters = substituters;
in {
  nix = {
    package = pkgs.nix;
    settings = {
      inherit substituters trusted-substituters;
      experimental-features = ["nix-command" "flakes"];
      # darwin: shits outside the litterbox.
      sandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
      # any user is allowed to use nix (as long as multi-user mode is used,
      # which it generally always is).
      allowed-users = ["*"];
      trusted-users = ["root" "@wheel" "@seadome"];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
    };

    gc = {
      automatic = true;
      dates = "weekly";
    };

    extraOptions = ''
      warn-dirty = false
    '';
  };
}
