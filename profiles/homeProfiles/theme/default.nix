# FIXME: need a berkeley mono profile
# FIXME: ensure firefox respects berkeley mono when it's not default
# FIXME: ensure kitty respects berkeley mono when it's not default
{flake, ...}: let
  inherit (flake.inputs.base16-schemes.lib) schemes;
  inherit (flake.self.lib.theme) mkColorScheme;
  l = flake.inputs.nixpkgs.lib // builtins;
in {
  imports = [
    ./__difftastic.nix
    ./__kitty.nix
  ];

  theme.enable = true;
  theme.color.schemes.dark = mkColorScheme schemes.tokyo-city-terminal-dark;
  theme.color.schemes.light = mkColorScheme schemes.tokyo-city-terminal-light;
  theme.fonts = {
    monospace = l.mkDefault {
      name = "Iosevka Term";
      # package = pkgs.iosevka-bin;
    };
    sansSerif = l.mkDefault {
      name = "Inter";
      # package = pkgs.inter;
    };
    serif = l.mkDefault {
      name = "IBM Plex Serif";
      # package = pkgs.ibm-plex;
    };
  };
}
