{ pkgs, lib, config, inputs, options, ... }:

with lib;
let
  inherit (config) my;
  inherit (my) xdg;

  home = my.user.home;
  cfg = my.modules.bash;
  bash = pkgs.bashInteractive_5;
  profileInit = "${config.dotfield.libDir}/profile.sh";
in
{
  options = with lib; {
    my.modules.bash = {
      enable = mkEnableOption ''
        Whether to enable bash module
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;
      enableCompletion = true;
      interactiveShellInit = "source ${xdg.config}/bash/bashrc";
    };

    my.env = { BASH_COMPLETION_USER_FILE = "${xdg.data}/bash/completion"; };
    my.user.packages = [ bash ];
    my.hm.xdg.configFile = {
      "bash/bashrc".text = ''
        # ${my.nix_managed}

        ${builtins.readFile profileInit}
      '';
    };

    environment = {
      shells = [ bash ];
      systemPackages = [ bash ];
      variables = {
        BASH_ENV = profileInit;
      };
    };
  };
}
