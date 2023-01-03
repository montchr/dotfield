{
  inputs,
  config,
  ...
}: let
  l = inputs.nixpkgs.lib // builtins;
  shellAbbrs = import ./abbrs.nix;
in {
  home.extraOutputsToInstall =
    ["/share/bash-completion"]
    ++ (l.optional config.programs.fish.enable "/share/fish")
    ++ (l.optional config.programs.zsh.enable "/share/zsh");
  home.shellAliases = import ./aliases.nix;

  programs.bash = {
    enable = true;
    sessionVariables = {
      BASH_COMPLETION_USER_FILE = "${config.xdg.dataHome}/bash/completion";
    };
  };

  programs.bash.shellAliases = shellAbbrs;
  programs.zsh.shellAliases = shellAbbrs;
  programs.fish = {inherit shellAbbrs;};

  programs.bottom.enable = true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.less.enable = true;
  programs.zoxide.enable = true;
}
