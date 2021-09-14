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
        user = { packages = with pkgs; [ direnv nix-direnv ]; };

        hm.configFile = {
          "direnv/direnvrc".text = ''
            # ${config.my.nix_managed}

            use_nvm() {
              local node_version="$1"

              nvm_sh="$NVM_DIR/nvm.sh"
              if [[ -e "$nvm_sh" ]]; then
                source "$nvm_sh"
                nvm use "$node_version"
              fi
            }

            # Bootstrap nix-direnv
            source ${pkgs.nix-direnv}/share/nix-direnv/direnvrc
          '';
        };
      };

      # https://github.com/nix-community/nix-direnv#via-configurationnix-in-nixos
      environment.pathsToLink = [ "/share/nix-direnv" ];
    };
}
