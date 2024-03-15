{
  flake,
  config,
  lib,
  pkgs,
  darwinProfiles,
  sharedProfiles,
  ...
}:
let
  inherit (flake.perSystem) packages;
  inherit (lib) optional;

  username = "cdom";

  hmCfg = config.home-manager.users.${username};
in
{
  imports = [
    darwinProfiles.builders.nixbuild-net
    darwinProfiles.yabai

    sharedProfiles.core.substituters.nixbuild-net
    sharedProfiles.secrets.default

    # FIXME: build failure on latest nixos-unstable as of [2023-07-05]
    # darwinProfiles.builders.vm-guest.default
  ];

  # FIXME: needs some tweaking upstream to account for nix-darwin...
  # imports = [inputs.klein-infra.darwinModules."aarch64-darwin".ssh-known-hosts];

  dotfield.users.cdom = { };
  dotfield.hosts.tuvix = {
    owner = config.dotfield.users.cdom;
  };

  # Allow nix-darwin to install the specified programs as applications.
  environment.systemPackages = optional hmCfg.programs.kitty.enable hmCfg.programs.kitty.package ++ [
    packages.synadm
  ];

  homebrew.casks = [
    "microsoft-teams"
    "onedrive"
  ];

  programs.fish.enable = true;

  users.users.${username} = {
    home = "/Users/${username}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  home-manager.users.${username} =
    { profiles, features, ... }:
    {
      imports = features.workstation ++ [
        # TODO: add to default profiles but darwin only
        profiles.os-specific.darwin.app-launcher-trampoline
        profiles.os-specific.darwin.yabai

        profiles.emacs.emacs-init
        profiles.shells.fish.default
      ];
      home.stateVersion = "22.05";
    };

  system.stateVersion = 4;
}
