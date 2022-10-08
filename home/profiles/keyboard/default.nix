{
  config,
  lib,
  pkgs,
  ...
}:
lib.mkMerge [
  {
    home.packages = with pkgs; [ ];
  }
  (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    services.skhd.enable = true;
    services.skhd.configPath = ./skhdrc;

    xdg.configFile."karabiner/karabiner.json".source = ./karabiner.json;
  })
]
