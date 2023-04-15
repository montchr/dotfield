{
  config,
  pkgs,
  inputs,
  self,
  packages,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config) theme xdg;
  inherit (inputs.apparat.lib.apps.kitty) makeConf makeThemeAttrs;
  l = inputs.nixpkgs.lib // builtins;
  colorScheme =
    if theme.colors.active != null
    then theme.colors.active
    else theme.colors.dark;
in {
  imports = [
    ./settings.nix
    ./symbols.nix
  ];

  home.packages = [
    packages.kitty-get-window-by-platform-id
  ];

  home.sessionVariables = {
    KITTY_CONFIG_DIRECTORY = "${xdg.configHome}/kitty";
  };

  programs.kitty = {
    enable = true;
    darwinLaunchOptions = l.mkIf isDarwin ["--single-instance"];
    settings = makeThemeAttrs colorScheme;
    keybindings = {
      # Open new tabs+windows from the current session's working directory.
      "kitty_mod+t" = "new_tab_with_cwd";
      "kitty_mod+enter" = "new_window_with_cwd";
    };
  };

  xdg.configFile = {
    "kitty/theme-dark.conf".text = makeConf (makeThemeAttrs theme.colors.dark);
    "kitty/theme-light.conf".text = makeConf (makeThemeAttrs theme.colors.light);
  };
}
