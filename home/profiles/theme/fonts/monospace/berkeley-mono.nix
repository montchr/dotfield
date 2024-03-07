{ config, flake, ... }:
let
  inherit (flake.perSystem.packages) berkeley-mono;
  cfg = config.theme.fonts;
in
{
  theme.fonts.monospace = {
    name = "Berkeley Mono";
    package = berkeley-mono;
    # $ kitty +list-fonts --psnames | grep Berkeley
    psNamespace = "BerkeleyMono";
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
