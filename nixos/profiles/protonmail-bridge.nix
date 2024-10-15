{
  lib,
  config,
  pkgs,
  ...
}:
{
  services.protonmail-bridge = {
    enable = true;
    path = [
      pkgs.pass
    ] ++ (lib.optional config.services.gnome.gnome-keyring.enable pkgs.gnome-keyring);
  };
}
