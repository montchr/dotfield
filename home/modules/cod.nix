{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkEnableOption types mkIf;
  cfg = config.services.cod;
in {
  options = {
    services.cod = {
      enable = mkEnableOption "Whether to enable the cod shell completion daemon.";

      package = mkOption {
        type = types.package;
        default = pkgs.cod;
        description = "Package providing cod.";
      };

      enableBashIntegration = mkOption {
        default = true;
        type = types.bool;
        description = ''
          Whether to enable Bash integration.
        '';
      };

      enableZshIntegration = mkOption {
        default = true;
        type = types.bool;
        description = ''
          Whether to enable Zsh integration.
        '';
      };

      enableFishIntegration = mkOption {
        default = true;
        type = types.bool;
        description = ''
          Whether to enable Fish integration.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [cfg.package];
    programs.bash.initExtra = mkIf cfg.enableBashIntegration ''
      source <(${cfg.package}/bin/cod init $$ bash)
    '';
    programs.fish.interactiveShellInit = mkIf cfg.enableFishIntegration ''
      ${cfg.package}/bin/cod init $fish_pid fish | source
    '';
    programs.zsh.initExtra = mkIf cfg.enableZshIntegration ''
      source <(${cfg.package}/bin/cod init $$ zsh)
    '';
  };
}
