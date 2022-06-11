{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.nur.repos.rycee) firefox-addons;

  cfg = config.programs.buku;
in {
  options = {
    programs.buku = {
      enable = lib.mkEnableOption "Whether to enable the module for the Buku bookmaking tool.";
      enableBrowserIntegration = lib.mkEnableOption "Whether to enable the Bukubrow native messaging host for browsers.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = [pkgs.buku];
    }
    (lib.mkIf cfg.enableBrowserIntegration {
      home.packages = [pkgs.bukubrow];
    })
    (lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin && cfg.enableBrowserIntegration) {
      # Darwin cannot use wrapped applications like NixOS, so the native
      # messaging service must be installed imperatively.
      home.activation.ensureBukubrowHost = lib.hm.dag.entryAfter ["writeBoundary"]
        ((lib.optionalString config.programs.firefox.enable ''
          $DRY_RUN_CMD ${pkgs.bukubrow}/bin/bukubrow --install-firefox
        '')
        + (lib.optionalString config.programs.chromium.enable ''
          $DRY_RUN_CMD ${pkgs.bukubrow}/bin/bukubrow --install-chromium
        ''));
    })
  ]);
}
