{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config) home;
in {
  imports = [../common.nix];
  programs.nushell = {
    enable = true;
  };
  xdg.configFile."nushell/home.nu".source = pkgs.writeText "${home.username}-home.nu" ''
    ${
      lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "$env.${name} = \"${value}\"") home.sessionVariables)
    }
  '';
}
