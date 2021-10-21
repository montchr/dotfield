{ config, lib, pkgs, ... }:

let
  inherit (lib.strings) fileContents;

  configDir = "${config.dotfield.configDir}/fish";

  mkFileLink = path: onChange: {
    "fish/${path}" = {
      inherit onChange;
      source = "${configDir}/${path}";
    };
  };
  mkFileLink' = path: mkFileLink "${path}.fish";
in
{
  imports = [
    ./abbrs.nix
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
    # FIXME: linking config results in overriding home-manager's control
    # (mkFileLink' "config")
    (mkFileLink "fish_plugins" "fisher update")
  ];

}
