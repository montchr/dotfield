# FIXME: restore kitty font config
# FIXME: ensure firefox respects berkeley mono when it's not default
# FIXME: pass `--color=16` to fzf
{flake, ...}: let
  inherit (flake.inputs.base16-schemes.lib) schemes;
  inherit (flake.self.lib.theme) mkColorScheme;
  l = flake.inputs.nixpkgs.lib // builtins;

  #: NOTE: Requires `--impure` flag.
  envColors = l.getEnv "DOTFIELD_COLORS";

  defaultColorSchemes = {
    dark = "default-dark";
    light = "one-light";
  };

  scheme =
    if (l.isString envColors && envColors != "")
    then defaultColorSchemes.${envColors} or envColors
    else defaultColorSchemes.dark;

  sessionVariables = {
    DOTFIELD_COLORS = scheme;
    DOTFIELD_COLORS_DARK = defaultColorSchemes.dark;
    DOTFIELD_COLORS_LIGHT = defaultColorSchemes.light;
    # DOTFIELD_THEME_MODE = cfg.colors.active.kind;
  };
in {
  theme.color.schemes.default = mkColorScheme schemes.solarized-dark;
  theme.color.schemes.dark = mkColorScheme schemes.solarized-dark;
  theme.color.schemes.light = mkColorScheme schemes.solarized-light;

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

  home = {inherit sessionVariables;};
  programs.bash = {inherit sessionVariables;};
  programs.zsh = {inherit sessionVariables;};

  programs.kitty.extraConfig = let
    # FIXME: should not hard-code proprietary font -- consider some lib mapping of known fonts to postscript names
    # $ kitty +list-fonts --psnames | grep <font-name>
    psName = "BerkeleyMono";
  in ''
    font_features ${psName}             -calt +dlig
    font_features ${psName}-Bold        -calt +dlig
    font_features ${psName}-Italic      -calt +dlig
    font_features ${psName}-BoldItalic  -calt +dlig
  '';

  # FIXME: The option `home-manager.users.cdom.specialization.dark.configuration._module.args.name' is defined multiple times.
  # Definition values:
  # - In `/nix/store/na16w66xsip7lzbvy4qlagfi3dz66cs9-source/modules/misc/specialization.nix': "cdom"
  # - In `<unknown-file>': "configuration"
  # specialization = {
  #   theme-dark.configuration = {config, ...}: {
  #     theme.color.schemes.default = l.mkForce config.theme.color.schemes.dark;
  #   };
  #   theme-light.configuration = {config, ...}: {
  #     theme.color.schemes.default = l.mkForce config.theme.color.schemes.light;
  #   };
  # };
}
