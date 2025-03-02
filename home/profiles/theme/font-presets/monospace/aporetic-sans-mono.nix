## Find PostScript font name for use in kitty terminal:
# $ kitty +list-fonts --psnames | grep Aporetic
# ...or with Ghostty:
# $ ghostty +list-fonts | grep Aporetic
{ config, pkgs, ... }:
let
  cfg = config.theme.fonts;
in
{
  theme.font.monospace = {
    name = "Aporetic Sans Mono";
    package = pkgs.aporetic;
    psNamespace = "Aporetic-Sans-Mono";
  };
  theme.font.terminal = cfg.monospace;
}
