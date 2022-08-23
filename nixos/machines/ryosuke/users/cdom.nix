{
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: {
  sops.secrets."users/cdom/passphrase".neededForUsers = true;

  users.users.cdom = {
    uid = 1000;
    isNormalUser = true;
    initialHashedPassword = "$6$XTIAlt33Lwfe309d$Zbthi9TYLdnxxKawGXzzX2QawlWkssJkYwP5NsxVb4430IRWz6TEtQfGdp5A9If5kRgj3BS2aacRsFxprfyKy.";
    passwordFile = config.sops.secrets."users/cdom/passphrase".path;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups =
      [
        "wheel"
        "video"
        "audio"
        "seadome"
        "secrets"
        "keys"
      ]
      ++ (lib.optional config.networking.networkmanager.enable "networkmanager")
      ++ (lib.optional config.services.mysql.enable "mysql")
      ++ (lib.optional config.virtualisation.docker.enable "docker")
      ++ (lib.optional config.virtualisation.podman.enable "podman")
      ++ (lib.optional config.virtualisation.libvirtd.enable "libvirtd")
      ++ (lib.optional config.virtualisation.virtualbox.host.enable "vboxusers");
    shell = pkgs.fish;
  };

  # Enable automatic login for the user.
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "cdom";

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  home-manager.users.cdom = hmArgs: {
    imports = with hmArgs.roles; workstation;
    home.stateVersion = "22.05";
    # FIXME: this must be set everywhere!
    programs.home-manager.enable = true;
  };
}
