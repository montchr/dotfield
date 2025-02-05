## Find PostScript font name for use in kitty terminal:
# $ kitty +list-fonts --psnames | grep Aporetic
# ...or with Ghostty:
# $ ghostty +list-fonts | grep Aporetic
{ config, flake, ... }:
let
  cfg = config.theme.fonts;
in
{
  theme.fonts.monospace = {
    name = "Aporetic Serif Mono";
    package = flake.perSystem.legacyPackages.aporetic.serif-mono;
    psNamespace = "Aporetic-Serif-Mono";
  };
  theme.fonts.terminal = cfg.monospace;

  programs.ghostty.settings.font-family = cfg.terminal.name;
  programs.ghostty.settings.font-feature = [
    "-calt"
    "+dlig"
  ];

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
