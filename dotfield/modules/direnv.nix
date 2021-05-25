{ pkgs, lib, config, ... }:
let
  cfg = config.my.modules.direnv;
  configDir = config.dotfield.configDir;
in
{
  options = with lib; {
    my.modules.direnv = {
      enable = mkEnableOption ''
        Whether to enable direnv module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my = {
        user = {
          packages = with pkgs; [
            direnv
            nix-direnv
          ];
        };

        hm.configFile = {
          "direnv/direnvrc".text = ''
            # Bootstrap nix-direnv
            source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
          '';
        };
      };

      environment.pathsToLink = [
        "/share/nix-direnv"
      ];
    };
}
