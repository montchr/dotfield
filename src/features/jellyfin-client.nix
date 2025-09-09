{
  aspects.graphical.home =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.jellyfin-media-player ];
    };
}
