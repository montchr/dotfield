{ pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    pure-prompt
  ] ++ lib.optionals stdenv.isDarwin [
    darwin-zsh-completions
  ];

  programs.zsh = {
    enable = true;

    cdpath = [
      "."
      "~"
    ];

    defaultKeymap = "viins";
    dotDir = ".config/zsh";

    history = {
      size = 50000;
      save = 500000;
      ignoreDups = true;
      share = true;
    };

    sessionVariables = {
      CLICOLOR = true;
      GPG_TTY = "$TTY";
      PATH = "$PATH:$HOME/.local/bin";
    };

    shellAliases = {
      restartaudio = "sudo killall coreaudiod";
      tf = "terraform";
    };

    profileExtra = ''
      export GPG_TTY=$(tty)
    '';

    # TODO: use my actual config
    initExtra = ''
      export KEYTIMEOUT=1

      bindkey "^?" backward-delete-char

      resume() {
        fg
        zle push-input
        BUFFER=""
        zle accept-line
      }
      zle -N resume
      bindkey "^Z" resume

      # function ls() {
      #     ${pkgs.coreutils}/bin/ls --color=auto --group-directories-first "$@"
      # }

      autoload -U promptinit; promptinit

      # Configure pure-promt
      prompt pure
      zstyle :prompt:pure:prompt:success color green
    '';
  };
}
