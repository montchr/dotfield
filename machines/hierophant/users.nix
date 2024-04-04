{ config, ops, ... }:
{
  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";
  users.mutableUsers = false;

  security.sudo.enable = true;

  security.sudo.wheelNeedsPassword = false;

  sops.secrets."users/cdom/hashed-password".neededForUsers = true;

  users.users.cdom = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      config.services.headscale.group
    ];
    hashedPasswordFile = config.sops.secrets."users/cdom/hashed-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
  };

  home-manager.users.cdom = _hmArgs: { home.stateVersion = "23.05"; };
}
