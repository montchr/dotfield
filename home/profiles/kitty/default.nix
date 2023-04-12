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
  };

  xdg.configFile = {
    "kitty/theme-dark.conf".text =
      makeConf (makeThemeAttrs theme.colors.dark);
    "kitty/theme-light.conf".text =
      makeConf (makeThemeAttrs theme.colors.light);

    # FIXME: does not appear to have an effect?
    "kitty/session".text = ''
      # Start new sessions in the previous working directory
      # https://sw.kovidgoyal.net/kitty/overview/#startup-sessions
      # https://sw.kovidgoyal.net/kitty/faq/#how-do-i-open-a-new-window-or-tab-with-the-same-working-directory-as-the-current-window
      launch --cwd=current
    '';
  };
}
