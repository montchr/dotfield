{ config, lib, ... }:
let
  l = import ./_lib.nix { inherit lib; };

  dotfieldDir = config.home.sessionVariables."DOTFIELD_DIR";
  DOTFIELD_USER_ZDOTDIR = "${dotfieldDir}/users/cdom/config/zsh";
in
{
  programs.zsh.initContent = l.mkInitUserConfig ''
    . "${DOTFIELD_USER_ZDOTDIR}/main.zsh"
  '';
}
