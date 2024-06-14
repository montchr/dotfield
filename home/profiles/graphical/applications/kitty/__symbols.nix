# TODO: ensure Symbols Nerd Font is installed
# TODO: give option to override, for patched fonts
{ lib, flake, ... }:
let
  inherit (builtins) attrValues map;
  inherit (flake.inputs) apparat;
  inherit (apparat.lib.kitty) makeConf makeSymbolsMap;
  inherit (apparat.constants.typography) glyphsets;
in
{
  # https://sw.kovidgoyal.net/kitty/conf/#fonts
  # https://old.reddit.com/r/KittyTerminal/comments/r5hssh/kitty_nerd_fonts/
  programs.kitty.extraConfig = makeConf {
    symbol_map = map (x: makeSymbolsMap x "Symbols Nerd Font") (attrValues glyphsets.symbols-nerd-font);
  };
}
