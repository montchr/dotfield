{
  inputs,
  config,
  lib,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
  shellAbbrs = import ./__shell.abbrs.nix;
  shellAliases = import ./__shell.aliases.nix;
in {
  programs.zsh.enable = lib.mkDefault true;

  # NOTE: due to `fish` shell's comprehensive completions,
  #       enabling it can slow down system rebuilds.
  programs.fish.enable = lib.mkDefault false;

  ##: listing/moving/paging
  programs.dircolors.enable = l.mkDefault true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.less.enable = true;
  programs.lesspipe.enable = l.mkDefault true;
  programs.zoxide.enable = true;

  # 'abbrs' (abbreviations) are essentially inline alias expansions --
  # traditional shell aliases only expand at the beginning of a line.
  # `fish` refers to 'abbrs' distinctly as 'abbrs',
  # though `zsh` also supports a similar mechanism(?).
  # TODO: zsh inline expansions with abbrs?
  home.shellAliases = shellAliases;
  programs.bash.shellAliases = shellAbbrs;
  programs.fish.shellAbbrs = shellAbbrs;
  programs.zsh.shellAliases = shellAbbrs;

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

    # Need to be after starship init since it overwrites PROMPT_COMMAND.
    initExtra = l.mkAfter ''
      ${l.optionalString (!config.programs.mcfly.enable) ''
        PROMPT_COMMAND="''${PROMPT_COMMAND:+''${PROMPT_COMMAND/%;*( )};}history -a"
        HISTTIMEFORMAT='%F %T '
      ''}

      # Must C-d at least thrice to close shell.
      export IGNOREEOF=2
    '';
  };

  home.extraOutputsToInstall =
    ["/share/bash-completion"]
    ++ (l.optional config.programs.fish.enable "/share/fish")
    ++ (l.optional config.programs.zsh.enable "/share/zsh");

  programs.readline = {
    enable = true;
    variables = {
      # Don't show hidden files unless a '.' is prefixed.
      match-hidden-files = false;

      # Expand tilde to home directory.
      expand-tilde = true;

      # Improve completion usability.
      completion-ignore-case = true;
      completion-prefix-display-length = 2;
      completion-map-case = true;

      # Avoid pressing TAB so much.
      show-all-if-ambiguous = true;
      show-all-if-unmodified = true;

      # Indicate file types.
      visible-stats = true;

      # Disable the internal pager.
      page-completions = false;
    };
  };
}
