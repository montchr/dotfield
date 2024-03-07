{ flake, pkgs, ... }:
let
  inherit (flake.perSystem.inputs') nixfmt-rfc101-style;
in
{
  home.packages = [
    nixfmt-rfc101-style.packages.default

    pkgs.alejandra
    pkgs.nix-init # <- generate nix package expressions from url
    pkgs.nix-melt # <- flake.lock explorer
    pkgs.nixpkgs-review
    pkgs.nvd # <- diff package changes between versions
  ];
}
