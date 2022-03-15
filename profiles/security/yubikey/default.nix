{
  config,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    yubikey-manager
    yubikey-personalization
  ];
}
