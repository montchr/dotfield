{
  pkgs,
  flake,
  config,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isAarch64;
  l = flake.inputs.nixpkgs.lib // builtins;
in
{
  environment.systemPackages = [ pkgs.bitwarden-cli ];
  # FIXME: electron marked as insecure, can't install (and considering the purpose of this package, don't make an exception to the security policy!)
  #     ++ (l.optional (config.services.xserver.enable && !isAarch64) pkgs.bitwarden);
}
