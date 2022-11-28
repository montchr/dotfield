{
  pkgs,
  inputs,
  config,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
in {
  environment.systemPackages =
    [pkgs.bitwarden-cli]
    ++ (l.optional config.services.xserver.enable pkgs.bitwarden);
}
