{ lib, pkgs, ... }:
let
  l = import ./_lib.nix { inherit lib; };
in
{
  imports = [ ./_custom-prompt.nix ];

  programs.zsh.initContent = l.mkInitConfigPreset ''
    source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
  '';
}
