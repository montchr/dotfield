{
  flake,
  config,
  ...
}: let
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellOptions = l.mkOptionDefault ["cdspell" "dirspell" "histreedit" "histverify"];
    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
    historyFileSize = 100000;
    historySize = 100000;
    historyControl = ["erasedups" "ignorespace"];
    historyIgnore = ["l" "x" "exit" "bg" "fg" "history" "poweroff" "ls" "cd" ".." "..."];

    initExtra = l.mkAfter ''
      # Needs to be after prompt init since it overwrites PROMPT_COMMAND.
      ${l.optionalString (!config.programs.mcfly.enable) ''
        PROMPT_COMMAND="''${PROMPT_COMMAND:+''${PROMPT_COMMAND/%;*( )};}history -a"
        HISTTIMEFORMAT='%F %T '
      ''}

      # Must C-d at least thrice to close shell.
      export IGNOREEOF=2
    '';
  };
}
