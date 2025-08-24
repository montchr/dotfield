{ moduleWithSystem, lib, ... }:
{
  dotfield.baseline.home =
    { config, ... }:
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
        historyFileSize = lib.mkDefault 100000;
        historySize = lib.mkDefault 100000;
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
          "."
          ".."
          "..."
          "...."
          "....."
        ];
      };
    };
}
