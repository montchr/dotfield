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
  ];

  theme.enable = true;
  theme.color.schemes.dark = mkColorScheme schemes.tokyo-city-terminal-dark;
  theme.color.schemes.light = mkColorScheme schemes.tokyo-city-terminal-light;
  theme.fonts = {
    monospace = lib.mkDefault {
      name = "JetBrains Mono";
      package = pkgs.jetbrains-mono;
    };
    sansSerif = lib.mkDefault {
      name = "Inter";
      package = pkgs.inter;
    };
    serif = lib.mkDefault {
      name = "IBM Plex Serif";
      package = pkgs.ibm-plex;
    };
  };
}
