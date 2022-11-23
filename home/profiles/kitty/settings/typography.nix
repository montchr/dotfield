{
  config,
  lib,
  inputs,
  self,
  ...
}: let
  inherit (self.lib.apps.kitty) makeFontFeatures';
  inherit (config) theme;
  l = inputs.nixpkgs.lib // builtins;

  iosevkaPostScriptSuffixes = [
    "Bold"
    "Bold-Extended"
    "Bold-Extended-Italic"
    "Bold-Extended-Oblique"
    "Bold-Italic"
    "Bold-Oblique"
    "Extended"
    "Extrabold"
    "Extralight"
    "Heavy"
    "Italic"
    "Light"
    "Medium"
    "Medium-Italic"
    "Regular"
    "Semibold"
    "Thin"
    "Thin-Italic"
  ];
in {
  programs.kitty.settings = {
    font_family = theme.fonts.term.family;
    font_size = "${l.toString theme.fonts.term.size}.0";
  };
  programs.kitty.extraConfig = makeFontFeatures' "Iosevka-Term" iosevkaPostScriptSuffixes ["-calt +dlig"];
}
