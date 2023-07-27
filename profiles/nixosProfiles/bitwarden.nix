{
  pkgs,
  flake,
  config,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  environment.systemPackages =
     [pkgs.bitwarden-cli]
     ++ (l.optional (config.services.xserver.enable && !isAarch64) pkgs.bitwarden);
}
