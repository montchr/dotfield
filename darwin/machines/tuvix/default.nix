# FIXME: make reusable -- duplicated as seadoom@hodgepodge
{
  self,
  config,
  lib,
  pkgs,
  primaryUser,
  ...
}: let
  inherit (self.inputs.digga.lib) rakeLeaves;
  inherit (config.lib.dotfield) srcPath;

  username = "cdom";

  # ownProfiles = rakeLeaves (srcPath + "/home/users/${username}/profiles");
  ownProfiles = rakeLeaves (srcPath + "/home/users/seadoom/profiles");
in {
  # sops.secrets."users/${username}/passphrase".neededForUsers = true;

  users.users.${username} = {
    home = "/Users/${username}";
    # isNormalUser = true;
    isHidden = false;
    shell = pkgs.fish;
    # openssh.authorizedKeys.keys = primaryUser.authorizedKeys;
  };

  home-manager.users.${username} = hmArgs: {
    imports =
      (with hmArgs.roles; workstation)
      ++ (with ownProfiles; [work]);
    home.stateVersion = "22.05";
  };
  system.stateVersion = 4;
}
