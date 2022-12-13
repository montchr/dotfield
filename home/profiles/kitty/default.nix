{
  config,
  pkgs,
  inputs,
  self,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  inherit (config) theme xdg;
  inherit (self.lib.apps.kitty) makeConf makeThemeAttrs;
  l = inputs.nixpkgs.lib // builtins;
in {
  imports = [./settings.nix];

  home.packages = with pkgs; [
    kitty-get-window-by-platform-id
  ];

  home.sessionVariables = {
    KITTY_CONFIG_DIRECTORY = "${xdg.configHome}/kitty";
  };

  programs.kitty = {
    enable = true;
    darwinLaunchOptions = l.mkIf isDarwin ["--single-instance"];
    settings = makeThemeAttrs {inherit (config.colorscheme) colors;};
  };

  xdg.configFile = {
    "kitty/theme-dark.conf".text =
      makeConf (makeThemeAttrs {inherit (theme.colors.dark) colors;});
    "kitty/theme-light.conf".text =
      makeConf (makeThemeAttrs {inherit (theme.colors.light) colors;});

    # FIXME: does not appear to have an effect?
    "kitty/session".text = ''
      # Start new sessions in the previous working directory
      # https://sw.kovidgoyal.net/kitty/overview/#startup-sessions
      # https://sw.kovidgoyal.net/kitty/faq/#how-do-i-open-a-new-window-or-tab-with-the-same-working-directory-as-the-current-window
      launch --cwd=current
    '';
  };
}
