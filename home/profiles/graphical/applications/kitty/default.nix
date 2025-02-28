{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (config) xdg;

  shellAliases = {
    # Use Kitty terminal"s ssh helper kitten
    sshk = "kitty +kitten ssh";
    # Display an image in kitty
    icat = "kitty +kitten icat";
  };
in
{
  imports = [
    ./__settings.nix
    ./__symbols.nix
  ];

  home = {
    sessionVariables."KITTY_CONFIG_DIRECTORY" = "${xdg.configHome}/kitty";
    inherit shellAliases;
  };

  # TODO: ugh
  # xdg.configFile = {
  #   "kitty/themes" = {
  #     source = flake.outPath + "/config/"
  #   };
  #   };

  programs.kitty = {
    enable = true;
    keybindings = {
      # Open new windows from the current session's working directory.
      # Tabs are intentionally left alone, to allow for choosing either
      # behavior.
      # FIXME: does not work well with ssh sessions
      "kitty_mod+enter" = "new_window_with_cwd";
    };
  };

  programs.fuzzel.settings.main.terminal = "kitty";
  wayland.windowManager.sway.config.terminal = "kitty";

  dconf.settings."org/cinnamon/desktop/applications/terminal" = {
    exec = "kitty";
  };
}
