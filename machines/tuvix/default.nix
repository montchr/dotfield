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
    # FIXME: build failure on latest nixos-unstable as of [2023-07-05]
    # darwinProfiles.builders.vm-guest.default

    sharedProfiles.secrets.default
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

  programs.fish.enable = true;

  users.users.${username} = {
    home = "/Users/${username}";
    isHidden = false;
    shell = pkgs.bashInteractive;
  };

  home-manager.users.${username} = {
    profiles,
    roles,
    ...
  }: {
    imports =
      roles.workstation
      ++ [
        profiles.shells.fish.default
        profiles.shells.fish.fzf-integration
        profiles.shells.fish.via-bash-init
        profiles.theme.fonts.monospace.jetbrains-mono
      ];
    home.stateVersion = "22.05";
  };

  system.stateVersion = 4;
}
