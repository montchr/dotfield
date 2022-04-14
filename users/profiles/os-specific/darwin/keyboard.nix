{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: {
  services.skhd.enable = true;
  # FIXME: avoid IFD -- update the module to support a path
  services.skhd.config = builtins.readFile "${pkgs.dotfield-config}/skhd/skhdrc";

  xdg.configFile."karabiner/karabiner.json".source = "${pkgs.dotfield-config}/karabiner/karabiner.json";
}
