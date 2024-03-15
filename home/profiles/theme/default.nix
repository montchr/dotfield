{
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (flake.inputs.base16-schemes.lib) schemes;
  inherit (flake.self.lib.theme) mkColorScheme;
in
{
  imports = [
    ./__difftastic.nix
    ./__kitty.nix

    ./font-presets/monospace/iosevka-comfy.nix
    ./font-presets/sans-serif/inter.nix
    ./font-presets/serif/ibm-plex-serif.nix
  ];

  theme.enable = true;
  theme.color.schemes.dark = mkColorScheme schemes.tokyo-city-terminal-dark;
  theme.color.schemes.light = mkColorScheme schemes.tokyo-city-terminal-light;

  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome.gnome-themes-extra;
  };
}
