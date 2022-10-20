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
  inherit (config.dotfield.paths) flakeRoot;
  inherit (lib) optional;

  username = "cdom";

  hmCfg = config.home-manager.users.${username};
  # hmApps = apps:  map (n: (optional hmCfg.programs.${n}.enable hmCfg.programs.${n}.package));
  ownProfiles = rakeLeaves (flakeRoot + "/home/users/${username}/profiles");
in {
  # Allow nix-darwin to install the specified programs as applications.
  environment.systemPackages =
    (optional hmCfg.programs.kitty.enable hmCfg.programs.kitty.package)
    ++ (optional hmCfg.programs.emacs.enable hmCfg.programs.emacs.package);

  users.users.${username} = {
    home = "/Users/${username}";
    isHidden = false;
    shell = pkgs.fish;
  };

  home-manager.users.${username} = hmArgs: {
    imports =
      (with hmArgs.roles; workstation)
      ++ (with ownProfiles; [work]);
    home.stateVersion = "22.05";
  };

  system.stateVersion = 4;
}
