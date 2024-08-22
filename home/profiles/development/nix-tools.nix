{ pkgs, ... }:
{
  home.packages = [
    pkgs.alejandra
    pkgs.nix-init # <- generate nix package expressions from url
    pkgs.nix-inspect # <- configuration inspector
    # XXX: broken with rust v1.80 <https://github.com/NixOS/nixpkgs/issues/332957>
    # pkgs.nix-melt # <- flake.lock explorer
    pkgs.nixfmt-rfc-style
    pkgs.nixpkgs-review
    pkgs.nvd # <- diff package changes between versions
  ];
}
