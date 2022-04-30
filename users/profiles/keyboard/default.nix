{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
lib.mkMerge [
  (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    services.skhd.enable = true;
    services.skhd.configPath = ./skhdrc;

    xdg.configFile."karabiner/karabiner.json".source = ./karabiner.json;
  })
]
