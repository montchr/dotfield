{
  dotfield.aspects.archivist.home =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.intermodal # torrent metainfo utility
        pkgs.monolith # <- bundle any web page into a single html file   => <https://github.com/Y2Z/monolith>
      ];
    };
}
