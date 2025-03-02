{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (flake.inputs.apparat.lib.color) reversePolarity;

  cfg = config.theme;
in
{
  imports = [
    ./__kitty.nix

  ];

  theme.enable = true;

  theme.font.sansSerif = {
    name = "Inter";
    package = pkgs.inter;
  };
  theme.font.serif = {
    name = "Aporetic Serif";
    package = pkgs.aporetic;
  };
  theme.font.monospace = {
    name = "Aporetic Sans Mono";
    package = pkgs.aporetic;
  };
  theme.font.terminal = cfg.font.monospace;

  home.packages = [ pkgs.fastfetch ];

  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome-themes-extra;
  };

  programs.bat.config.theme = "base16";
  programs.git.delta.options.features = "base16";
  programs.git.delta.options.light = cfg.color.scheme.default.kind == "light";
  programs.git.difftastic.background = reversePolarity cfg.theme.color.scheme.default.kind;
  programs.ghostty.settings = {
    font-family = cfg.font.terminal.name;
    font-size = 11;
    theme = "dark:catppuccin-frappe,light:catppuccin-latte";
  };
}
