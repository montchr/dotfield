flake@{ ... }:
{
  dotfield.users.cdom.aspects.graphical.home =
    { pkgs, ... }:
    {
      imports = [ flake.config.dotfield.aspects.graphical.home ];

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
