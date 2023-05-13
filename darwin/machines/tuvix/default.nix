# FIXME: make reusable -- duplicated as seadoom@hodgepodge
{
  self,
  config,
  lib,
  pkgs,
  darwinProfiles,
  sharedProfiles,
  packages,
  ...
}: let
  inherit (self.inputs.digga.lib) rakeLeaves;
  inherit (lib) optional;

  username = "cdom";

  hmCfg = config.home-manager.users.${username};
  # hmApps = apps:  map (n: (optional hmCfg.programs.${n}.enable hmCfg.programs.${n}.package));
  ownProfiles = rakeLeaves (self + "/home/users/cdom/profiles");
in {
  imports = [
    darwinProfiles.builders.vm-guest
    sharedProfiles.fonts.berkeley-mono
  ];
  # FIXME: needs some tweaking upstream to account for nix-darwin...
  # imports = [inputs.klein-infra.darwinModules."aarch64-darwin".ssh-known-hosts];

  dotfield.users.cdom = {};
  dotfield.hosts.tuvix = {
    owner = config.dotfield.users.cdom;
  };

  # Allow nix-darwin to install the specified programs as applications.
  environment.systemPackages =
    (optional hmCfg.programs.kitty.enable hmCfg.programs.kitty.package)
    ++ (optional hmCfg.programs.emacs.enable hmCfg.programs.emacs.package);

  users.users.${username} = {
    home = "/Users/${username}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  home-manager.users.${username} = hmArgs: {
    imports =
      hmArgs.roles.workstation
      ++ [ownProfiles.work];
    # FIXME: incorrect hosts ip configuration!
    # ++ [inputs.klein-infra.homeManagerModules.${system}.ssh-config];
    home.stateVersion = "22.05";
    # FIXME: temporarily to avoid merge conflict
    home.packages = [hmArgs.packages.ast-grep];
  };

  system.stateVersion = 4;
}
