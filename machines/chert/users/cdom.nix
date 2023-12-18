{
  config,
  ops,
  pkgs,
  ...
}: {
  sops.secrets.user-cdom-password.neededForUsers = true;

  users.users.cdom = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    hashedPasswordFile = config.sops.secrets."user-cdom-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    # Loads fish shell on interactive init.
    shell = pkgs.bashInteractive;
  };

  home-manager.users.cdom = {profiles, ...}: {
    imports = [
      # Include common mail profile for testing purposes.
      profiles.mail.default

      profiles.shells.fish.default
    ];
    home.stateVersion = "23.05";
  };
}
