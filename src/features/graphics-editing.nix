{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.gimp-with-plugins
        pkgs.inkscape
      ];
    };
}
