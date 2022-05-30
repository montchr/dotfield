{ config, lib, pkgs, ... }:
let
  inherit (pkgs.nur.repos.rycee) firefox-addons;

  cfg = config.programs.buku;
in
{
  options = {
    programs.buku = {
      enable = lib.mkEnableOption "Whether to enable the module for the Buku bookmaking tool.";
      enableBrowserIntegration = lib.mkEnableOption "Whether to enable the Bukubrow native messaging host for browsers.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.buku
      (lib.mkIf cfg.enableBrowserIntegration pkgs.bukubrow)
    ];
  };
}
