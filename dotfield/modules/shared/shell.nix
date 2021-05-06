# This is handcrafted setup to keep the same performance characteristics I had
# before using nix or even improve it. Simple rules followed here are:
#
# - Setup things as early as possible when the shell runs
# - Inline files when possible instead of sourcing then
# - User specific shell files are to override or for machine specific setup

{ pkgs, lib, config, inputs, options, ... }:

let
  dotfield = config.dotfield;
  home = config.my.user.home;

  cfg = config.my.modules.shell;

  # TODO: necessary? coreutils should certainly exist already
  darwinPackages = with pkgs; [ openssl gawk gnused coreutils findutils ];
in {
  options = with lib; {
    my.modules.shell = {
      enable = mkEnableOption ''
        Whether to enable shell module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable (mkMerge [{

      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment = {
        shells = [ pkgs.bashInteractive_5 pkgs.zsh ];

        systemPackages = with pkgs;
          (if stdenv.isDarwin then darwinPackages else nixosPackages) ++ [
            # TODO: not yet
            # cachix
            curl
            direnv
            fzf
            htop
            nix-zsh-completions
            rsync
            wget
            z-lua
            zsh
          ];
      };

      my = {
        env = {
          ZDOTDIR            = "$XDG_CONFIG_HOME/zsh";
          ZSH_CACHE          = "$XDG_CACHE_HOME/zsh";

          # zinit
          ZPFX               = "$HOME/.local";
          ZINIT_HOME         = "$XDG_DATA_HOME/zsh";
          ZINIT_BIN_DIR_NAME = "bin";
        };

        hm = {
          configFile = {
            "shell" = {
              recursive = true;
              source    = "${dotfield.configDir}/shell";
            };

            "zsh" = {
              recursive = true;
              source    = "${dotfield.configDir}/zsh";
            };
          };

          dataFile = {
            "zsh/plugins/_local---config" = {
              recursive = true;
              source    = "${dotfield.configDir}/zsh/config";
            };
          };
        };

        user = {
          shell = if pkgs.stdenv.isDarwin then [ pkgs.zsh ] else pkgs.zsh;
          packages = with pkgs; [
            # TODO: gzip: Payload.gz: No such file or directory
            # _1password # CLI
            asciinema
            bandwhich # display current network utilization by process
            bottom # fancy version of `top` with ASCII graphs
            cacert
            # TODO later
            # cachix
            coreutils
            curl
            exa
            fd
            findutils
            gawk
            getopt
            gnumake
            gnupg
            gnused
            gnutar
            grc
            gpgme
            htop
            hyperfine
            jq
            less
            lnav # System Log file navigator
            ncdu
            # nix-zsh-completions
            nodePackages.node2nix
            # nodePackages.vim-language-server
            pandoc
            pass
            # plantuml
            pywal
            rename # might not work
            # TODO: is this the perl rename, or the less-useful one?
            renameutils
            ripgrep
            rsync
            shellcheck
            shfmt
            # TODO: any additional setup needed to have this replace tldr? also, why?
            tealdeer # rust implementation of `tldr`
            # tldr
            tmux
            # TODO: unar is "unsupported" on darwin?
            # unar
            # TODO: what is this? i keep seeing it
            # universal-ctags
            # TODO: investigate
            # urlscan
            vim
            vim-vint
            wget
            yq
          ];
        };


      };

      programs.zsh = {
        enable = true;
        enableCompletion = false;
      };
    }]);
}
