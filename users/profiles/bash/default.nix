{ pkgs, lib, config, inputs, options, ... }:

let
  inherit (config) my;
  inherit (my) xdg;

  configDir = "${xdg.config}/bash";

  shellCfg = config.shell;
  bash = pkgs.bashInteractive_5;
in
{
  imports = [
    ../shell
  ];

  programs.bash = {
    enableCompletion = true;
    interactiveShellInit = "source ${configDir}/bashrc";
  };

  my.env = { BASH_COMPLETION_USER_FILE = "${xdg.data}/bash/completion"; };
  my.hm.xdg.configFile = {
    "bash/env".text = ''
      # ${my.nix_managed}

      ${shellCfg.envInit}
    '';

    "bash/bashrc".text = ''
      # ${my.nix_managed}

      source "${configDir}/env"

      ${shellCfg.rcInit}
    '';
  };

  environment = {
    shells = [ bash ];
    systemPackages = [ bash ];
    # variables = {
    #   BASH_ENV = profileInit;
    # };
  };
}
