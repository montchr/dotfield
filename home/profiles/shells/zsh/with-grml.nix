{ lib, pkgs, ... }:
let
  l = import ./lib.nix { inherit lib; };
in
{
  imports = [ ./custom-prompt.nix ];

  programs.zsh.initExtraFirst = l.mkInitConfigPreset ''
    source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
  '';
}
