{
  theme.fonts = {
    monospace = {
      name = "Berkeley Mono";
      # package = pkgs.iosevka-bin;
    };
    terminal = {
      name = "Berkeley Mono";
      # package = pkgs.iosevka-bin;
    };
  };

  # TODO: consider some lib mapping of known fonts to postscript names so this can be automated
  programs.kitty.extraConfig = let
    # $ kitty +list-fonts --psnames | grep <font-name>
    psName = "BerkeleyMono";
  in ''
    font_features ${psName}             -calt +dlig
    font_features ${psName}-Bold        -calt +dlig
    font_features ${psName}-Italic      -calt +dlig
    font_features ${psName}-BoldItalic  -calt +dlig
  '';
}
