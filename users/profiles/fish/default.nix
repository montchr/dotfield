{ config, lib, pkgs, ... }:

let
  inherit (lib.strings) fileContents;

  configDir = "${config.dotfield.configDir}/fish";

  mkFileLink = path: { "fish/${path}".source = "${configDir}/${path}"; };
  mkFileLink' = path: mkFileLink "${path}.fish";
in
{
  imports = [
    ./abbrs.nix
    # ./plugins.nix
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
  };

  my.hm.xdg.configFile = lib.mkMerge [
    (mkFileLink' "config")
    (mkFileLink "fish_plugins")
  ];

}
