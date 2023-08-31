{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (config.home) username;
  # FIXME: hardcoded
  userConfigDir = "~/.config/dotfield/users/${username}/config/nushell";
in {
  imports = [../common.nix];
  # Handle manually.
  programs.direnv.enableNushellIntegration = false;
  programs.nushell = {
    enable = true;
    extraConfig = ''
      source ${userConfigDir}/config.nu
    '';
    # extraEnv = ''
    #   source ${userConfigDir}/env.nu
    # '';
    # configFile.source = ./config.nu;
    # envFile.source = ./env.nu;
    # extraConfig = builtins.readFile ./config.nu;
  };
}
