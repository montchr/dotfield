{ lib, pkgs, ... }:
let
  shellAliases = import ./aliases.nix { inherit pkgs; };
  shellAbbrs = import ./abbrs.nix { inherit pkgs; };
in
{
  imports = [ ./bash ];

  home = {
    inherit shellAliases;
  };

  programs.bash.shellAliases = shellAbbrs;
  programs.zsh.shellAliases = shellAbbrs;
  programs.fish = {
    inherit shellAbbrs;
  };
  # FIXME: `home.shellAliases` and `home.sessionVariables` are not propagated
  #        into the nushell session.  we get by without `home.sessionVariables`
  #        thanks to the trampoline.
  programs.nushell.shellAliases = shellAbbrs // shellAliases;

  programs.dircolors.enable = true;

  programs.bat.enable = true;
  programs.bottom.enable = true;
  programs.carapace.enable = true;
  programs.eza.enable = true;
  programs.info.enable = true;
  programs.less.enable = true;
}
