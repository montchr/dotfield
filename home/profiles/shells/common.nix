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

  # Magical shell history
  # FIXME: https://github.com/nix-community/home-manager/issues/3511
  # programs.atuin.enable = true;
  programs.atuin.settings = {
    auto_sync = true;
    sync_frequency = "10m";
    sync_address = "https://api.atuin.sh";
    search_mode = "fuzzy"; # 'prefix' | 'fulltext' | 'fuzzy'
    filter_mode = "global"; # 'global' | 'host' | 'session' | 'directory'
  };

  programs.bottom.enable = true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.less.enable = true;
  programs.zoxide.enable = true;
}
