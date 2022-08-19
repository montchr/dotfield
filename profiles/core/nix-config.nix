{
  config,
  lib,
  pkgs,
  ...
}: {
  nix = {
    package = pkgs.nix;
    settings = {
      sandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
      # FIXME: dangerous
      allowed-users = ["*"];
      trusted-users = ["root" "@wheel" "@seadome"];

      substituters = [
        "https://cache.nixos.org/"
        "https://dotfield.cachix.org"
        "https://nix-community.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
      ];

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

    # FUP Options {{
    # https://github.com/gytis-ivaskevicius/flake-utils-plus/blob/166d6ebd9f0de03afc98060ac92cba9c71cfe550/lib/options.nix
    linkInputs = true;
    generateRegistryFromInputs = true;
    generateNixPathFromInputs = true;
    # }}
  };
}
