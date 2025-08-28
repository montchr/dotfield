{ pkgs, ... }:
{
  home.packages = [
    pkgs.nix-init
    pkgs.nix-inspect # <- configuration inspector
    pkgs.nixfmt-rfc-style
    pkgs.nixpkgs-review
  ];
}
