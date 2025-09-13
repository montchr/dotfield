{ moduleWithSystem, lib, ... }:
{
  users.cdom.aspects.core.home = moduleWithSystem (
    perSystem@{ config }:
    { config, pkgs, ... }:
    let
      inherit (perSystem.config.packages) fzf-tab-completion;

      shellAbbrs = import ./__abbrs.nix { inherit pkgs; };
    in
    {
      programs.bash = {
        shellAliases = shellAbbrs;
        shellOptions = lib.mkOptionDefault [
          "cdspell"
          "dirspell"
          "histreedit"
          "histverify"
        ];
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
  );
}
