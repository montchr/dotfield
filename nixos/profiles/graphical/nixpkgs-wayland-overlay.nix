{ flake, ... }:
{
  nix.settings = {
    substituters = [ "https://nixpkgs-wayland.cachix.org" ];
    trusted-public-keys = [
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
  };
  nix.settings.trusted-substituters = [ "https://nixpkgs-wayland.cachix.org" ];
}
