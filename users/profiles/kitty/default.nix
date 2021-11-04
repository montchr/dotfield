{ pkgs, lib, config, options, inputs, ... }:
with lib;
let
  inherit (inputs) base16-kitty;
  inherit (config) my colorscheme;
  inherit (colorscheme) colors;

  # cfg = my.modules.kitty;
  themesCfg = my.modules.themes;
  configDir = "${config.dotfield.configDir}/kitty";
  socket = "unix:/tmp/kitty-socket";

  kitty-get-window-by-platform-id =
    (pkgs.writeShellScriptBin "kitty-get-window-by-platform-id" ''
      kitty @ --to $KITTY_SOCKET ls \
        | ${pkgs.jq}/bin/jq -r --argjson id "$1" \
          '.[] | select(.platform_window_id==$id)'
    '');
in
{
  my.user.packages = [
    # pkgs.kitty
    kitty-get-window-by-platform-id
  ];

  my.env = {
    KITTY_CONFIG_DIRECTORY = "${my.xdg.config}/kitty";
    KITTY_SOCKET = socket;
    TERMINFO_DIRS =
      if pkgs.stdenv.isDarwin then
        "/Applications/kitty.app/Contents/Resources/kitty/terminfo"
      else
      # FIXME causes build failure on darwin due to beautifulsoup unit test failure (but it should not fail)
        "${pkgs.kitty.terminfo.outPath}/share/terminfo";
  };

  my.hm.programs.kitty = {
    enable = true;

    darwinLaunchOptions = [
      "--single-instance"
      "--listen-on=${socket}"
    ];

    extraConfig = ''
      font_features PragmataProMonoLiga-Regular +calt
      font_features PragmataProMonoLiga-Italic +calt
      font_features PragmataProMonoLiga-BoldItalic +calt
    '';

    settings = (import ./settings.nix { inherit config lib; }) //
      (import ./colors.nix { inherit (config.colorscheme) colors; }) //
      {
        allow_remote_control = "yes";
        listen_on = socket;
      };
  };

  my.hm.xdg.configFile = {
    "kitty/base16-kitty".source = base16-kitty.outPath;
    "kitty/session".text = "cd ~";
  };
}
