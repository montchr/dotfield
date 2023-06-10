{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkMerge mkPackageOption;
  cfg = config.programs.nix-doc;
in {
  options.programs.nix-doc = {
    enable = mkEnableOption "nix-doc";
    package = mkPackageOption pkgs "nix-doc" {};
    plugin = {
      enable = mkEnableOption "Nix CLI integration plugin for nix-doc";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [cfg.package];
    }
    (mkIf cfg.plugin.enable {
      # FIXME: inconsistent `.so`/`.dylib` ext on darwin? not my wheelhouse...
      nix.settings.plugin-files = ["${cfg.package}/lib/libnix_doc_plugin.so"];
    })
  ]);
}
