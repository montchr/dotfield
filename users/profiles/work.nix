{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.nur.repos.rycee) firefox-addons;
in {
  programs.firefox.extensions = with firefox-addons; [
    lastpass-password-manager
  ];
}
