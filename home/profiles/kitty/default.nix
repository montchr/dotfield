{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
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

  programs.kitty = {
    enable = true;
    # TODO: prob don't need the conditional
    darwinLaunchOptions = lib.mkIf isDarwin [ "--single-instance" ];
    keybindings = {
      # Open new windows from the current session's working directory.
      # Tabs are intentionally left alone, to allow for choosing either behavior.
      "kitty_mod+enter" = "new_window_with_cwd";
    };
  };
}
