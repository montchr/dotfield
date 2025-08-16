{
  dotfield.users.cdom.features.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [
        ##: color utils
        pkgs.colorpanes # <- print panes in the 8 bright terminal colors with shadows of the respective darker color
        pkgs.sanctity # <- ruSt ANsi16 Color Test utIliTY
        (pkgs.writeShellApplication {
          name = "color-panic";
          runtimeInputs = [ pkgs.colorpanes ];
          text = ''
            colorpanes --captions --height 38 --width 24
          '';
        })
      ];
    };
}
