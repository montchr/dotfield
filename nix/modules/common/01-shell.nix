{ pkgs, lib, config, inputs, ... }:

let
  cfg = config.my.modules.shell;
  home = config.my.user.home;
  darwinPackages = with pkgs; [ openssl gawk gnused coreutils findutils ];
  linuxPackages = with pkgs; [ dwm dmenu xclip ];
in
{
  options = with lib; {
    my.modules.shell = {
      enable = mkEnableOption ''
        Whether to enable shell module
      '';
    };
  };

  config = with lib;
    mkIf cfg.enable {

      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment = {
        shells = [
          pkgs.bashInteractive_5
          pkgs.zsh
        ];
        variables = {
          XDG_CACHE_HOME = "${home}/.cache";
          XDG_CONFIG_HOME = "${home}/.config";
          XDG_DATA_HOME = "${home}/.local/share";
        };
        systemPackages = with pkgs;
          (if stdenv.isDarwin then darwinPackages else nixosPackages) ++ [
            cachix
            curl
            direnv
            fzf
            htop
            nix-zsh-completions
            rsync
            wget
            z
            zsh
          ];
      };

      my = {
        user = {
          shell = if pkgs.stdenv.isDarwin then [ pkgs.zsh ] else pkgs.zsh;
          packages = with pkgs; [
            _1password # CLI
            bandwhich # display current network utilization by process
            bat
            bottom # fancy version of `top` with ASCII graphs
            # buku
            docker
            exa
            fd
            # glow # md renderer?
            grc
            hyperfine
            jq
            lnav # System Log file navigator
            mosh
            ncdu
            pandoc
            pass
            rename
            shellcheck
            shfmt
            tealdeer # rust implementation of `tldr`
            tokei # code statistics
          ];
        };

        hm.file = {
          # TODO: why not `hm.configFile`?
          ".config/zsh" = {
            recursive = true;
            source = ../../../config/zsh.d/zsh;
          };
          ".terminfo" = {
            recursive = true;
            source = ../../../config/.terminfo;
          };
        };

        env =
          # ====================================================
          # This list gets set in alphabetical order.
          # So care needs to be taken if two env vars depend on each other
          # ====================================================
          rec {
            COLORTERM = "truecolor";
            BROWSER = if pkgs.stdenv.isDarwin then "open" else "xdg-open";
            # # Better spell checking & auto correction prompt
            # SPROMPT =
            #   "zsh: correct %F{red}'%R'%f to %F{blue}'%r'%f [%B%Uy%u%bes, %B%Un%u%bo, %B%Ue%u%bdit, %B%Ua%u%bbort]?";
            # Set the default Less options.
            # Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
            # Remove -X and -F (exit if the content fits on one screen) to enable it.
            LESS = "-F -g -i -M -R -S -w -X -z-4";
            KEYTIMEOUT = "1";
            ZDOTDIR = "$XDG_CONFIG_HOME/zsh";

            DOTFIELD = "$HOME/.config";
            PROJECTS = "$HOME/Developer";
            WORK = "$HOME/broadway";
            PERSONAL_STORAGE = "$HOME/Sync";
            NOTES_DIR = "${PERSONAL_STORAGE}/org";

            ############### APPS/PROGRAMS XDG SPEC CLEANUP
            AWS_SHARED_CREDENTIALS_FILE = "$XDG_CONFIG_HOME/aws/credentials";
            AWS_CONFIG_FILE = "$XDG_CONFIG_HOME/aws/config";
            DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
            ELINKS_CONFDIR = "$XDG_CONFIG_HOME/elinks";

            ############### Telemetry
            DO_NOT_TRACK = "1"; # Future proof? https://consoledonottrack.com/
            HOMEBREW_NO_ANALYTICS = "1";
            GATSBY_TELEMETRY_DISABLED = "1";
            ADBLOCK = "true";

            ############### Autosuggest
            ZSH_AUTOSUGGEST_USE_ASYNC = "true";


            VIM_FZF_LOG = ''
              "$(${pkgs.git}/bin/git config --get alias.l 2>/dev/null | awk '{$1=""; print $0;}' | tr -d '\r')"'';
          };

      };

      programs.zsh = {
        enable = true;
        enableCompletion = true;

        ########################################################################
        # Instead of sourcing, I can read the files & save startiup time instead
        ########################################################################

        # zshenv
        # TODO: why zsh.d ?
        # shellInit = builtins.readFile ../../../config/zsh.d/.zshenv;
        shellInit = builtins.readFile ../../../config/zsh/.zshenv;

        # zshrc
        interactiveShellInit = lib.concatStringsSep "\n"
          (map builtins.readFile [
            # TODO: revisit this approach
            # ../../../config/zsh.d/zsh/config/options.zsh
            # ../../../config/zsh.d/zsh/config/input.zsh
            # ../../../config/zsh.d/zsh/config/completion.zsh
            # ../../../config/zsh.d/zsh/config/utility.zsh
            # ../../../config/zsh.d/zsh/config/aliases.zsh
            # "${pkgs.grc}/etc/grc.zsh"
            # "${pkgs.fzf}/share/fzf/completion.zsh"
            # "${pkgs.fzf}/share/fzf/key-bindings.zsh"
            # "${z}/share/z.sh"
            ../../../config/zsh/.zshrc
          ]);

        # promptInit = "autoload -U promptinit; promptinit; prompt pure";
      };#
    };
}
