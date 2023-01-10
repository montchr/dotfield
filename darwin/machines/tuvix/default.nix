# FIXME: make reusable -- duplicated as seadoom@hodgepodge
{
  self,
  inputs,
  config,
  pkgs,
  ...
}: let
  inherit (inputs.flib.lib.modules) rakeLeaves;
  l = inputs.nixpkgs.lib // builtins;

  username = "cdom";

  hmCfg = config.home-manager.users.${username};
  # hmApps = apps:  map (n: (optional hmCfg.programs.${n}.enable hmCfg.programs.${n}.package));
  ownProfiles = rakeLeaves (self + "/home/users/cdom/profiles");
in {
  dotfield.users = {
    cdom = {
    };
  };
  dotfield.hosts.tuvix = {
    owner = config.dotfield.users.cdom;
  };

  # Allow nix-darwin to install the specified programs as applications.
  environment.systemPackages =
    (l.optional hmCfg.programs.kitty.enable hmCfg.programs.kitty.package)
    ++ (l.optional hmCfg.programs.emacs.enable hmCfg.programs.emacs.package);

  users.users.${username} = {
    home = "/Users/${username}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  home-manager.users.${username} = hmArgs: {
    imports =
      (with hmArgs.roles; workstation)
      ++ (with ownProfiles; [work]);
    home.stateVersion = "22.05";
  };

  system.stateVersion = 4;
}
