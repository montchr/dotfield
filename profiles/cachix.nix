{
  nix.binaryCaches = [
    "https://cachix.org/api/v1/cache/dotfield"
    "https://cachix.org/api/v1/cache/emacs"
    "https://nrdxp.cachix.org"
    "https://cachix.org/api/v1/cache/nix-community"
    "https://cache.nixos.org/"
  ];
  nix.binaryCachePublicKeys = [
    "dotfield.cachix.org-1:b5H/ucY/9PDARWG9uWA87ZKWUBU+hnfF30amwiXiaNk="
    "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
    "nrdxp.cachix.org-1:Fc5PSqY2Jm1TrWfm88l6cvGWwz3s93c6IOifQWnhNW4="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];
}
