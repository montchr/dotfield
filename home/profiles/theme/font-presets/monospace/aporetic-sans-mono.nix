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
    name = "Aporetic Sans Mono";
    package = flake.perSystem.legacyPackages.aporetic.sans-mono;
    psNamespace = "Aporetic-Sans-Mono";
  };
  theme.fonts.terminal = cfg.monospace;
}
