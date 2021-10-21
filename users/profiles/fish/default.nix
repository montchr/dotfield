{ config, lib, pkgs, ... }:

let
  inherit (lib.strings) fileContents;

  configDir = "${config.dotfield.configDir}/fish";
in
{
  imports = [
    ./abbrs.nix
    ./plugins.nix
  ];

  environment.variables = {
    SHELL = "fish";
  };

  my.user.shell = pkgs.fish;

  my.hm.programs = {
    fish = {
      enable = true;
      # interactiveShellInit = fileContents ./interactiveShellInit.fish;
      # interactiveShellInit = ''
      # '';
      # shellInit = fileContents ./shellInit.fish;
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
