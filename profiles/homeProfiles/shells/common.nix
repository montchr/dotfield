{
  flake,
  config,
  ...
}: let
  l = flake.inputs.nixpkgs.lib // builtins;
  shellAbbrs = import ./abbrs.nix;
in {
  imports = [
    ./bash
    ./readline.nix
  ];

  home.extraOutputsToInstall =
    ["/share/bash-completion"]
    ++ (l.optional config.programs.fish.enable "/share/fish")
    ++ (l.optional config.programs.zsh.enable "/share/zsh");

  home.shellAliases = import ./aliases.nix;

  programs.bash.shellAliases = shellAbbrs;
  programs.nushell.shellAliases = shellAbbrs;
  programs.zsh.shellAliases = shellAbbrs;
  programs.fish = {inherit shellAbbrs;};

  programs.bat.enable = true;
  programs.bottom.enable = true;
  programs.dircolors.enable = l.mkDefault true;
  programs.exa.enable = true;
  programs.exa.enableAliases = true;
  programs.info.enable = l.mkDefault true;
  programs.less.enable = true;
  programs.lesspipe.enable = l.mkDefault true;
  programs.zoxide.enable = true;
}
