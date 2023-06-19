{
  config,
  lib,
  pkgs,
  darwinProfiles,
  sharedProfiles,
  ...
}: let
  inherit (lib) optional;

  username = "cdom";

  hmCfg = config.home-manager.users.${username};
in {
  imports = [
    darwinProfiles.builders.vm-guest.default
    sharedProfiles.fonts.berkeley-mono
  ];

  # FIXME: needs some tweaking upstream to account for nix-darwin...
  # imports = [inputs.klein-infra.darwinModules."aarch64-darwin".ssh-known-hosts];

  dotfield.users.cdom = {};
  dotfield.hosts.tuvix = {
    owner = config.dotfield.users.cdom;
  };

  # Allow nix-darwin to install the specified programs as applications.
  environment.systemPackages = optional hmCfg.programs.kitty.enable hmCfg.programs.kitty.package;

  homebrew.casks = ["microsoft-teams" "onedrive"];

  users.users.${username} = {
    home = "/Users/${username}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  home-manager.users.${username} = {
    profiles,
    roles,
    ...
  }: {
    imports =
      roles.workstation
      ++ [profiles.theme.fonts.berkeley-mono];
    # FIXME: incorrect hosts ip configuration!
    # ++ [inputs.klein-infra.homeManagerModules.${system}.ssh-config];
    home.stateVersion = "22.05";
  };

  system.stateVersion = 4;
}
