{
  lib,
  pkgs,
  ...
}:
lib.mkMerge [
  {
    home.packages = with pkgs; [];
  }
  (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    services.skhd.enable = true;
    services.skhd.configPath = ./skhdrc;

    # FIXME: new version of karabiner-elements will always move a
    # store-linked/read-only file because it expects to write to its own
    # configuration via gui
    # xdg.configFile."karabiner/karabiner.json".source = ./karabiner.json;
  })
]
