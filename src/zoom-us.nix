# Tracking issue: <https://github.com/NixOS/nixpkgs/issues/107495>
{
  flake.modules.nixos.workstation =
    {
      lib,
      pkgs,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) isAarch64;
    in
    {
      # XXX: broken package on aarch64-linux
      environment.systemPackages = lib.optionals (!isAarch64) [ pkgs.zoom-us ];
    };

}
