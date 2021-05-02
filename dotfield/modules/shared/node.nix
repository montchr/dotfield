{ pkgs, lib, config, inputs, ... }:

let

  dotfield = config.dotfield;
  cfg = config.my.modules.node;

in {
  options = with lib; {
    my.modules.node = {
      enable = mkEnableOption ''
        Whether to enable node module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my = {
        env = {
          # ############## Direnv & n
          # # N_PREFIX = "$XDG_DATA_HOME";
          # NODE_VERSIONS = "$XDG_DATA_HOME/n/versions/node";
          NODE_VERSION_PREFIX = "";
        };

        user = {
          packages = with pkgs; [
            nodejs # LTS
            nodePackages.npm
            (yarn.override { inherit nodejs; })
            dePackages.svgo
          ];
        };

        hm.file = {
          ".npmrc" = with config.my; {
            text = ''
              # ${nix_managed}
              # vim:ft=conf
              ${lib.optionalString (email != "") "email=${email}"}
              init-license=MIT
              ${lib.optionalString (email != "") "init-author-email=${email}"}
              ${lib.optionalString (name != "") "init-author-name=${name}"}
              ${lib.optionalString (website != "") "init-author-url=${website}"}
              init-version=0.0.1
              ${builtins.readFile builtins.toPath /.
              "${dotfield.configDir}/npm/npmrc"}
            '';
          };
        };
      };
    };
}
