{
  aspects.graphical.home =
    { lib, ... }:
    let
      inherit (lib.hm.gvariant) mkTuple;
    in
    {
      home.keyboard.options = [ "compose:ralt" ];

      dconf.settings."org/gnome/desktop/input-sources" = {
        sources = [
          (mkTuple [
            "xkb"
            "us"
          ])
        ];
        xkb-options = [
          "terminate:ctrl_alt_bksp"
          "compose:ralt"
        ];
      };
    };
}
