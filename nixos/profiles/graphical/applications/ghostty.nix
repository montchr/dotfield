# XXX: Will not work on Asahi Linux until Zig adds support for 16K
# page size: https://github.com/ziglang/zig/pull/20511
{ flake, ... }:
{
  environment.systemPackages = [
    flake.perSystem.inputs'.ghostty.packages.default
  ];

  nix.settings = {
    substituters = [ "https://ghostty.cachix.org" ];
    trusted-substituters = [ "https://ghostty.cachix.org" ];
    trusted-public-keys = [ "ghostty.cachix.org-1:QB389yTa6gTyneehvqG58y0WnHjQOqgnA+wBnpWWxns=" ];
  };
}
