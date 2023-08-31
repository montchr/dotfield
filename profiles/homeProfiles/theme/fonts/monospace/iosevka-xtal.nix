#: Find PostScript font name for use in kitty terminal:
#: $ kitty +list-fonts --psnames | grep Iosevka Xtal
{
  config,
  flake,
  ...
}: let
  inherit (flake.perSystem.packages) iosevka-xtal iosevka-xtal-term;
  cfg = config.theme.fonts;
in {
  theme.fonts.monospace = {
    name = "Iosevka Xtal";
    package = iosevka-xtal;
    psNamespace = "Iosevka-Xtal";
  };
  theme.fonts.terminal = {
    name = "Iosevka Xtal Term";
    package = iosevka-xtal-term;
    psNamespace = "Iosevka-Xtal-Term";
  };

  programs.kitty.extraConfig = let
    inherit (cfg.terminal) psNamespace;
  in ''
    # NOTE: Iosevka's "Regular" weight has no PostScript suffix
    font_features ${psNamespace}             -calt +dlig
    font_features ${psNamespace}-Bold        -calt +dlig
    font_features ${psNamespace}-Italic      -calt +dlig
    font_features ${psNamespace}-BoldItalic  -calt +dlig
  '';
}
