{
  nix.settings = {
    substituters = [ "https://nixpkgs-update.cachix.org/" ];
    trusted-public-keys = [
      "nixpkgs-update.cachix.org-1:6y6Z2JdoL3APdu6/+Iy8eZX2ajf09e4EE9SnxSML1W8="
    ];
  };
}
