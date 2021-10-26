{ pkgs, lib, config, inputs, options, ... }:

with lib;
let
  cfg = config.my.modules.zsh;
  configDir = "${config.dotfield.configDir}/zsh";
  envInit = "${config.dotfield.libDir}/profile.sh";

  # TODO: copied from settings.nix because they're not available here! get them from the same place.
  # see: https://github.com/hlissner/dotfiles/blob/master/lib/options.nix
  mkOpt = type: default: mkOption { inherit type default; };
  mkOpt' = type: default: description:
    mkOption { inherit type default description; };
in
{
  options = with lib; {
    my.modules.zsh = with types; {
      enable = mkEnableOption ''
        Whether to enable zsh module
      '';

      aliases = mkOpt (attrsOf (either str path)) { };

      rcInit = mkOpt' lines "" ''
        Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshrc and sourced by
        $XDG_CONFIG_HOME/zsh/.zshrc
      '';
      envInit = mkOpt' lines "" ''
        Zsh lines to be written to $XDG_CONFIG_HOME/zsh/extra.zshenv and sourced
        by $XDG_CONFIG_HOME/zsh/.zshenv
      '';

      rcFiles = mkOpt (listOf (either str path)) [ ];
      envFiles = mkOpt (listOf (either str path)) [ ];
    };
  };

  config = with lib;
    mkIf cfg.enable {
      my.user.shell = pkgs.zsh;

      programs.zsh = {
        enable = true;
        enableCompletion = false;
        enableBashCompletion = false;
        promptInit = "";
      };

      my.env = rec {
        # zsh paths
        ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
        ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
        ZSH_DATA = "$XDG_DATA_HOME/zsh";

        # zgenom paths
        ZGEN_DIR = "$XDG_DATA_HOME/zsh/sources";
        ZGEN_SRC_DIR = "$XDG_DATA_HOME/zsh/zgenom";
      };

      my.hm.xdg.configFile = {
        "zsh" = {
          source = configDir;
          recursive = true;
          onChange = ''
            # Remove compiled files.
            fd -uu \
              --extension zwc \
              --exec \
                rm '{}'
          '';
        };

        "zsh/extra.zshrc".text =
          let
            aliasLines =
              mapAttrsToList (n: v: ''alias ${n}="${v}"'') cfg.aliases;
          in
          ''
            # ${config.my.nix_managed}

            ${concatStringsSep "\n" aliasLines}

            ${concatMapStrings (path: "source '${path}'") cfg.rcFiles}

            ${cfg.rcInit}
          '';

        "zsh/profile.zshenv".source = envInit;

        "zsh/extra.zshenv".text = ''
          # ${config.my.nix_managed}

          ${concatMapStrings (path: "source '${path}'") cfg.envFiles}

          ${cfg.envInit}
        '';
      };

    };
}
