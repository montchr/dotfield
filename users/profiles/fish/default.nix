{ config, lib, pkgs, ... }:

let
  inherit (lib.strings) fileContents;

  configDir = "${config.dotfield.configDir}/fish";
in
{
  environment.variables = {
    SHELL = "fish";
  };

  my.user.shell = pkgs.fish;

  my.hm.programs = {
    fish = {
      enable = true;
      # interactiveShellInit = fileContents ./interactiveShellInit.fish;
      interactiveShellInit = ''
        set fish_greeting
      '';
      shellInit = fileContents ./shellInit.fish;
      shellAbbrs = import ./abbrs.nix;
    };

    zoxide.enable = true;
  };

  my.hm.xdg.configFile = {
    "fish" = {
      source = configDir;
      recursive = true;
    };
  };

}
