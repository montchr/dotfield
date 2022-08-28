{
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: {
  sops.secrets."users/seadoom/passphrase".neededForUsers = true;
  users.users.seadoom = {
    uid = 1000;
    isNormalUser = true;
    passwordFile = config.sops.secrets."users/seadoom/passphrase".path;
    openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
    extraGroups =
      [
        "wheel"
        "video"
        "audio"
        "networkmanager"
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
  };
  home-manager.users.seadoom = hmArgs: {imports = with hmArgs.roles; workstation;};
}
