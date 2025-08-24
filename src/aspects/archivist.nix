{
  dotfield.aspects.archivist.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.intermodal # torrent metainfo utility
      ];
    };
}
