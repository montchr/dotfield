#: SS08 => PragmataPro style
#
#: Find PostScript font name for use in kitty terminal:
#: $ kitty +list-fonts --psnames | grep Iosevka
{ config, pkgs, ... }:
let
  cfg = config.theme.fonts;
in
{
  theme.font.monospace = {
    name = "Iosevka SS08";
    package = pkgs.iosevka-bin;
    psNamespace = "Iosevka-SS08";
  };
  theme.font.terminal = {
    name = "Iosevka Term SS08";
    package = pkgs.iosevka-bin;
    psNamespace = "Iosevka-Term-SS08";
  };

  programs.kitty.extraConfig =
    let
      inherit (cfg.terminal) psNamespace;
    in
    ''
      # NOTE: Iosevka's "Regular" weight has no PostScript suffix
      font_features ${psNamespace}             -calt +dlig
      font_features ${psNamespace}-Bold        -calt +dlig
      font_features ${psNamespace}-Italic      -calt +dlig
      font_features ${psNamespace}-BoldItalic  -calt +dlig
    '';
}
