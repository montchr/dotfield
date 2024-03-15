#: Find PostScript font name for use in kitty terminal:
#: $ kitty +list-fonts --psnames | grep Iosevka Comfy
{ config, pkgs, ... }:
let
  cfg = config.theme.fonts;
in
{
  theme.fonts.sansSerif = {
    name = "Iosevka Comfy Wide Duo";
    package = pkgs.iosevka-comfy.comfy-wide-duo;
    # TODO: might be wrong
    psNamespace = "Iosevka-Comfy-Wide-Duo";
  };
  theme.fonts.serif = {
    name = "Iosevka Comfy Wide Motion Duo";
    package = pkgs.iosevka-comfy.comfy-wide-motion-duo;
    # TODO: might be wrong
    psNamespace = "Iosevka-Comfy-Wide-Motion-Duo";
  };
  theme.fonts.monospace = {
    name = "Iosevka Comfy";
    package = pkgs.iosevka-comfy.comfy;
    psNamespace = "Iosevka-Comfy";
  };
  theme.fonts.terminal = cfg.monospace;

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
