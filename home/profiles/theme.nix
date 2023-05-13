{
  config,
  pkgs,
  self,
  ...
}: let
  cfg = config.theme;
  sessionVariables = {
    DOTFIELD_COLORS = cfg.colors.active;
    DOTFIELD_COLORS_DARK = cfg.colors.dark;
    DOTFIELD_COLORS_LIGHT = cfg.colors.light;
  };
in {
  theme.colors = {
    dark = "classic-dark";
    # dark = getColorScheme "da-one-gray";
    # dark = getColorScheme "black-metal-khold";
    light = "default-light";
    # FIXME: has some pretty unreadable contrast, esp. yellow on white
    # light = getColorScheme "one-light";
  };

  theme.fonts = {
    # NOTE: variable font doesn't seem to be compatible with normal weight
    #       handling in Firefox on macOS. firefox, for example,
    #       renders *everything* in bold. not sure whether it's the
    #       font or the application.
    mono.family = "Berkeley Mono";
    term = cfg.fonts.mono;
    sans.family = "Inter";
    serif.family = "IBM Plex Serif";
    symbols.family = "Symbols Nerd Font Mono";
  };

  # NOTE: as of <2023-05-11>, `programs.fish` doesn't declare `sessionVariables`.
  home.sessionVariables = sessionVariables;
  programs.bash = {inherit sessionVariables;};
  programs.zsh = {inherit sessionVariables;};

  home.packages = [
    ##: color utils
    pkgs.colorpanes #  <- print panes in the 8 bright terminal colors with shadows of the respective darker color
    pkgs.sanctity #    <- ruSt ANsi16 Color Test utIliTY
    pkgs.pastel #      <- generate, analyze, convert and manipulate colors
    (pkgs.writeShellApplication {
      name = "color-panic";
      runtimeInputs = [pkgs.colorpanes];
      text = ''
        colorpanes --captions --height 38 --width 24
      '';
    })
  ];

  # FIXME: The option
  # `home-manager.users.cdom.specialization.dark.configuration._module.args.name'
  # is defined multiple times.
  #        My guess is that this is due to calling evalModules twice? How else
  #        would we get multiple definitions?

  # Definition values:
  # - In `/nix/store/na16w66xsip7lzbvy4qlagfi3dz66cs9-source/modules/misc/specialization.nix': "cdom"
  # - In `<unknown-file>': "configuration"
  # specialization = {
  #   dark.configuration = {
  #     theme.colors.active = colors.dark;
  #     colorScheme = colors.dark;
  #   };
  #   light.configuration = {
  #     theme.colors.active = colors.light;
  #     colorScheme = colors.light;
  #   };
  # };
}
