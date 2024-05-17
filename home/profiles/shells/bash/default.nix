# NOTE: This file is imported by `../common.nix`, so that should not be imported here.
#       This differs from the other shell profiles, which do the reverse.
#       Bash is essential, so will always be configured.
{
  flake,
  config,
  lib,
  ...
}:
let
  inherit (flake.perSystem.packages) fzf-tab-completion;
in
{
  home.extraOutputsToInstall = [ "/share/bash-completion" ];

  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellOptions = lib.mkOptionDefault [
      "cdspell"
      "dirspell"
      "histreedit"
      "histverify"
    ];
    sessionVariables = {
      "BASH_COMPLETION_USER_FILE" = "${config.xdg.dataHome}/bash/completion";
    };
    historyFileSize = 100000;
    historySize = 100000;
    historyControl = [
      "erasedups"
      "ignorespace"
    ];
    historyIgnore = [
      "l"
      "x"
      "exit"
      "bg"
      "fg"
      "history"
      "poweroff"
      "ls"
      "cd"
      ".."
      "..."
    ];

    initExtra = lib.mkAfter ''
      # Must C-d at least thrice to close shell.
      export IGNOREEOF=2

      # fzf tab completion interface
      source ${fzf-tab-completion}/share/bash/fzf-bash-completion.sh
      bind -x '"\t": fzf_bash_completion'
    '';
  };
}
