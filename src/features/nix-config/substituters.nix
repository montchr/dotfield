{
  aspects.core.nixos = { lib, ... }: {
    # Fallback quickly if substituters are not available.
    nix.settings.connect-timeout = lib.mkDefault 5;
    nix.settings.fallback = true;

    # Avoid copying unnecessary stuff over SSH.
    nix.settings.builders-use-substitutes = true;

    nix.settings.substituters = [
      "https://cache.numtide.com"
      "https://dotfield.cachix.org"
      "https://nix-community.cachix.org"
    ];
    nix.settings.trusted-substituters = [
      "ssh://eu.nixbuild.net"
      "https://nixpkgs-update.cachix.org/"
    ];
    nix.settings.trusted-public-keys = [
      "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
      "nixbuild.net/cdom-1:DU7hcG2k5kj9nC6NUvsOYQNiaI5UXYjjY5gBOccaND4="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8="
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };
}
