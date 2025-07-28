{
  dotfield.modules.archivist.nixos =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.intermodal # torrent metainfo utility
      ];
    };
}
