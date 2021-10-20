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
      programs.zsh = {
        enable = true;
        enableCompletion = false;
        enableBashCompletion = false;
        promptInit = "";
      };

      my.env = rec {
        # Default is "1". But when typeset in PragmataPro that leaves no space
        # between the icon and its filename.
        EXA_ICON_SPACING = "2";

        # zsh paths
        ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
        ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
        ZSH_DATA = "$XDG_DATA_HOME/zsh";

        # zgenom paths
        ZGEN_DIR = "$XDG_DATA_HOME/zsh/sources";
        ZGEN_SRC_DIR = "$XDG_DATA_HOME/zsh/zgenom";

        # zoxide
        _ZO_DATA_DIR = "$XDG_DATA_HOME/zoxide";
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

      my.user = {
        shell = if pkgs.stdenv.isDarwin then [ pkgs.zsh ] else pkgs.zsh;
        packages = with pkgs; [
          # TODO: gzip: Payload.gz: No such file or directory
          # _1password # CLI
          act # Run GitHub Actions locally
          asciinema
          # bandwhich # display current network utilization by process
          # TODO: "not packaged for macOS yet"
          # bitwarden
          cacert
          circleci-cli
          du-dust
          getopt
          grex # Generate regexps from user-provided test cases
          hyperfine
          lnav # System Log file navigator
          mcfly
          # FIXME: Doesn't exist!
          # nomino #  Batch rename utility for developers
          pandoc
          podman
          shellcheck
          shfmt
          # TODO: unar is "unsupported" on darwin?
          # unar
          universal-ctags
          vim-vint
          yamllint
          yq
          zoxide
        ];
      };
    };
}
