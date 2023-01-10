{
  inputs,
  ...

}: let
  inherit (inputs.flib.lib.apps.kitty) makeConf makeSymbolsMap;
  l = inputs.nixpkgs.lib // builtins;
  fontName = "Symbols Nerd Font";
  glyphSets = import ./nerdfont-glyph-sets.nix;
in {
  # https://sw.kovidgoyal.net/kitty/conf/#fonts
  # https://old.reddit.com/r/KittyTerminal/comments/r5hssh/kitty_nerd_fonts/
  programs.kitty.extraConfig = makeConf {
    symbol_map = l.map (x: makeSymbolsMap x fontName) (l.attrValues glyphSets);
  };
}
