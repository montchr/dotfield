{ lib, ... }:
let
  inherit (lib.hm.gvariant) mkTuple;
in
{
  home.keyboard.options = [ "compose:ralt" ];

  dconf.settings."org/gnome/desktop/input-sources" = {
    sources = lib.singleton (mkTuple [
      "xkb"
      "us"
    ]);
    xkb-options = [
      "caps:ctrl_modifier"
      "terminate:ctrl_alt_bksp"
      "compose:ralt"
    ];
  };
}
