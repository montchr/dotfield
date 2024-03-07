#: Find PostScript font name for use in kitty terminal:
#: $ kitty +list-fonts --psnames | grep <font-name>
{ config, pkgs, ... }:
let
  cfg = config.theme.fonts;
in
{
  theme.fonts.monospace = {
    name = "JetBrains Mono";
    package = pkgs.jetbrains-mono;
    psNamespace = "JetBrainsMono";
  };
  theme.fonts.terminal = cfg.monospace;

  programs.kitty.extraConfig =
    let
      inherit (cfg.terminal) psNamespace;
    in
    ''
      font_features ${psNamespace}-Regular     -calt +dlig
      font_features ${psNamespace}-Bold        -calt +dlig
      font_features ${psNamespace}-Italic      -calt +dlig
      font_features ${psNamespace}-BoldItalic  -calt +dlig
    '';
}
