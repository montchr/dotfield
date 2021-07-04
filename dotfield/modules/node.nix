{ pkgs, lib, config, inputs, ... }:

let cfg = config.my.modules.node;
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
            nodePackages.eslint
            nodePackages.node2nix
            nodePackages.npm
            nodePackages.prettier
            nodePackages.stylelint
            nodePackages.svgo
            (yarn.override { inherit nodejs; })
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
            '';
          };

          ".prettierrc" = ''
            # ${nix_managed}
            ---
            arrowParens: 'always'
            bracketSpacing: true
            jsxBracketSameLine: false
            jsxSingleQuote: false
            printWidth: 80
            proseWrap: 'never'
            quoteProps: 'as-needed'
            semi: true
            singleQuote: true
            tabWidth: 2
            trailingComma: 'all'
            useTabs: false
            overrides:
              - files: '*.php'
                options:
                  braceStyle: '1tbs'
                  phpVersion: '7.4'
                  tabWidth: 4
                  trailingCommaPHP: true
                  useTabs: true
            '';
        };
      };
    };
}
