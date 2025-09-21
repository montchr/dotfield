{
  aspects.workstation.home =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.plex-desktop ];
    };
}
