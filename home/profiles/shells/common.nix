{
  flake,
  config,
  pkgs,
  ...
}:
let
  l = flake.inputs.nixpkgs.lib // builtins;
  shellAliases = import ./aliases.nix { inherit pkgs; };
  shellAbbrs = import ./abbrs.nix { inherit pkgs; };
in
{
  imports = [ ./bash ];

  home = {
    inherit shellAliases;
    extraOutputsToInstall =
      [ "/share/bash-completion" ]
      ++ (l.optional config.programs.fish.enable "/share/fish")
      ++ (l.optional config.programs.zsh.enable "/share/zsh");
  };

  programs.bash.shellAliases = shellAbbrs;
  programs.zsh.shellAliases = shellAbbrs;
  programs.fish = {
    inherit shellAbbrs;
  };
  # FIXME: does not inherit these or sessionVariables.
  #        we get by without the sessionVariables
  #        thanks to the trampoline.
  #        but aliases don't come with.
  programs.nushell.shellAliases = shellAbbrs // shellAliases;

  programs.dircolors.enable = l.mkDefault true;
  programs.carapace.enable = true;

  programs.bat.enable = true;
  programs.bottom.enable = true;
  programs.eza.enable = true;
  programs.info.enable = l.mkDefault true;
  programs.less.enable = true;
  programs.lesspipe.enable = l.mkDefault true;
}
