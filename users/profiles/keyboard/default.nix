{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
lib.mkMerge [
  {
    home.packages = with pkgs; [
      # https://ergodox-ez.com/pages/wally-planck
      wally-cli
    ];
  }
  (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    services.skhd.enable = true;
    services.skhd.configPath = ./skhdrc;

    xdg.configFile."karabiner/karabiner.json".source = ./karabiner.json;
  })
]
