{
  config,
  ops,
  pkgs,
  ...
}: let
  username = "anomich";
in {
  sops.secrets.user-anomich-password.neededForUsers = true;

  users.users.${username} = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    hashedPasswordFile = config.sops.secrets."user-${username}-password".path;
    openssh.authorizedKeys.keys = ops.users.cdom.keys.default;
    # Loads fish shell on interactive init.
    shell = pkgs.bashInteractive;
  };

  home-manager.users.${username} = hmArgs: {
    imports = [
      hmArgs.profiles.shells.fish.trampoline
    ];
    home.packages = [];
    home.stateVersion = "23.05";
  };
}
