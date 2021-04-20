{ pkgs, lib, config, inputs, ... }:

let
  cfg = config.my.modules.node;
  # n = pkgs.callPackage ../../pkgs/n.nix { source = inputs.n; };
in
{
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
          N_PREFIX = "$XDG_DATA_HOME";
          NODE_VERSIONS = "$XDG_DATA_HOME/n/versions/node";
          NODE_VERSION_PREFIX = "";
        };

        user = {
          packages = with pkgs; [
            # n
            nodejs # LTS
            nodePackages.npm
            (yarn.override { inherit nodejs; })
            # TODO: add more
            nodePackages.svgo
          ];
        };

        hm.file = {
          ".npmrc" = with config.my; {
            text = ''
              # ${nix_managed}
              # vim:ft=conf
              ${lib.optionalString (email != "") "email=${email}"}
              # TODO: use GPLv3
              # init-license=MIT
              ${lib.optionalString (email != "") "init-author-email=${email}"}
              ${lib.optionalString (name != "") "init-author-name=${name}"}
              ${lib.optionalString (website != "") "init-author-url=${website}"}
              init-version=0.0.1
              # TODO: this could be better... where's a more appropriate location?
              ${builtins.readFile ../../../config/.npmrc}'';
          };
        };
      };
    };
}
