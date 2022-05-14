{config, lib, pkgs, ...}:
{
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    displayManager.gdm.wayland = true;
    desktopManager.gnome.enable = true;
  };

  services.gnome = {
    gnome-keyring.enable = true;
    chrome-gnome-shell.enable = true;
  };

  services.dbus = {
    enable = true;
    packages = with pkgs; [ gnome.dconf ];
  };

  services.udev = {
    packages = with pkgs; [ gnome.gnome-settings-daemon ];
    # TODO: wut https://github.com/Icy-Thought/Snowflake/blob/ff12f668d4ec66019e388a66c0efba8ff8a34e01/modules/desktop/gnome.nix#L33-L40
    # extraRules = ''
    #   ACTION=="add|change", KERNEL=="nvme[0-9]*", ATTR{queue/scheduler}="none"
    #   ACTION=="add|change", KERNEL=="sd[a-z]|mmcblk[0-9]*", ATTR{queue/rotational}=="0", ATTR{queue/scheduler}="mq-deadline"
    #   ACTION=="add|change", KERNEL=="sd[a-z]", ATTR{queue/rotational}=="1", ATTR{queue/scheduler}="bfq"
    # '';
  };

  environment.systemPackages = with pkgs.gnomeExtensions; [
    gsconnect
  ];

  xdg.portal.extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  xdg.portal.gtkUsePortal = true;

  qt5.platformTheme = lib.mkForce "gnome";
}

## === sources ===
# https://github.com/Icy-Thought/Snowflake/blob/ff12f668d4ec66019e388a66c0efba8ff8a34e01/modules/desktop/gnome.nix
