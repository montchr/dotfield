{ pkgs, lib, config, inputs, options, ... }:

with lib;
let
  cfg = config.my.modules.zsh;
  envInit = "${config.dotfield.libDir}/profile.sh";

  # TODO: copied from settings.nix because they're not available here! get them from the same place.
  # see: https://github.com/hlissner/dotfiles/blob/master/lib/options.nix
  mkOpt = type: default: mkOption { inherit type default; };
  mkOpt' = type: default: description:
    mkOption { inherit type default description; };

  # TODO: necessary? coreutils should certainly exist already
  darwinPackages = with pkgs; [ openssl gawk gnused coreutils findutils ];
in {
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
        # Default is "1" but with PragmataPro that leaves no space between the
        # icon and its filename.
        EXA_ICON_SPACING="2";

        ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
        ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
        ZSH_DATA = "$XDG_DATA_HOME/zsh";

        ZGEN_DIR = "$XDG_DATA_HOME/zsh/sources";
        ZGEN_SRC_DIR = "$XDG_DATA_HOME/zsh/zgenom";
      };

      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment = {
        shells = [ pkgs.zsh ];

        systemPackages = with pkgs;
          (if stdenv.isDarwin then darwinPackages else nixosPackages) ++ [
            bottom
            cachix
            coreutils
            curl
            direnv
            fzf
            gcc
            gnumake
            grc
            lua
            manix # nix documentation search
            rsync
            wget
            z-lua
            zsh
          ];
      };

      my.hm = {
        configFile = {
          "zsh/extra.zshrc".text = let
            aliasLines =
              mapAttrsToList (n: v: ''alias ${n}="${v}"'') cfg.aliases;
          in ''
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

      my.user = {
        shell = if pkgs.stdenv.isDarwin then [ pkgs.zsh ] else pkgs.zsh;
        packages = with pkgs; [
          # TODO: gzip: Payload.gz: No such file or directory
          # _1password # CLI
          asciinema
          # bandwhich # display current network utilization by process
          # TODO: "not packaged for macOS yet"
          # bitwarden
          bottom # fancy version of `top` with ASCII graphs
          cacert
          cachix
          dua # ncdu alternative :: https://github.com/Byron/dua-cli
          exa
          fd
          findutils
          gawk
          getopt
          gnupg
          gnused
          gnutar
          grex # Generate regexps from user-provided test cases
          hyperfine
          lua
          jq
          less
          lnav # System Log file navigator
          # FIXME: Doesn't exist!
          # nomino #  Batch rename utility for developers
          pandoc
          pass
          ripgrep
          rsync
          shellcheck
          shfmt
          tmux
          # TODO: unar is "unsupported" on darwin?
          # unar
          # TODO: what is this? i keep seeing it
          # universal-ctags
          vim
          vim-vint
          wget
          yq
          zsh
        ];
      };
    };
}
