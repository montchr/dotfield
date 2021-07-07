{ pkgs, lib, config, ... }:
let
  cfg = config.my.modules.direnv;
  configDir = config.dotfield.configDir;
in {
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
            # https://github.com/nix-community/nix-direnv#shell-integration

            function nixify() {
              if [ ! -e ./.envrc ]; then
                echo "use nix" > .envrc
                direnv allow
              fi
              if [[ ! -e shell.nix ]] && [[ ! -e default.nix ]]; then
                cat > default.nix <<'EOF'
            with import <nixpkgs> {};
            mkShell {
              nativeBuildInputs = [
                bashInteractive
              ];
            }
            EOF
                ${EDITOR:-vim} default.nix
              fi
            }

            function flakifiy() {
              if [ ! -e flake.nix ]; then
                nix flake new -t github:nix-community/nix-direnv .
              elif [ ! -e .envrc ]; then
                echo "use flake" > .envrc
                direnv allow
              fi
              ${EDITOR:-vim} flake.nix
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
