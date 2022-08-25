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
  programs.ssh.matchBlocks = lib.mkAfter {
    "kweb-prod-www" = {
      hostname = "67.225.164.90";
      # FIXME
      # port = 522;
      user = "cdom";
    };
    "kweb-prod-db" = {
      hostname = "67.225.164.91";
      port = 522;
      user = "cdom";
    };
    "kweb-dev" = {
      hostname = "67.43.11.196";
      user = "cdom";
    };
  };
}
