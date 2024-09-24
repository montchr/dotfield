{ pkgs, ... }:
{
  security.polkit.enable = true;
  services = {
    accounts-daemon.enable = true;
    dbus.packages = [
      pkgs.gcr
      pkgs.gnome-settings-daemon
    ];

    # Ref: <https://github.com/NixOS/nixpkgs/blob/3030f185ba6a4bf4f18b87f345f104e6a6961f34/nixos/modules/services/x11/desktop-managers/gnome.nix#L395>
    gvfs.enable = true;
  };
}
