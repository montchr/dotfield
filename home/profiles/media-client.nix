{ lib, ... }:
let
  inherit (builtins) listToAttrs map;

  mkTypeAssocs =
    ftypes: id:
    (listToAttrs (
      map (name: {
        inherit name;
        value = lib.singleton id;
      }) ftypes
    ));
in
{
  imports = [
    ./mpv.nix
    ./jellyfin-client.nix
    ./plex-client.nix
  ];

  xdg.mimeApps.defaultApplications =
    let
      audioTypes = [
        "audio/x-vorbis+ogg"
        "audio/aac"
        "audio/x-aac"
        "audio/vnd.dolby.heaac.1"
        "audio/vnd.dolby.heaac.2"
        "audio/aiff"
        "audio/x-aiff"
        "audio/m4a"
        "audio/x-m4a"
        "audio/mp1"
        "audio/x-mp1"
        "audio/mp2"
        "audio/x-mp2"
        "audio/mp3"
        "audio/x-mp3"
        "audio/mpeg"
        "audio/mpeg2"
        "audio/mpeg3"
        "audio/mpegurl"
        "audio/x-mpegurl"
        "audio/mpg"
        "audio/x-mpg"
        "audio/rn-mpeg"
        "audio/musepack"
        "audio/x-musepack"
        "audio/ogg"
        "audio/scpls"
        "audio/x-scpls"
        "audio/vnd.rn-realaudio"
        "audio/wav"
        "audio/x-pn-wav"
        "audio/x-pn-windows-pcm"
        "audio/x-realaudio"
        "audio/x-pn-realaudio"
        "audio/x-ms-wma"
        "audio/x-pls"
        "audio/x-wav"
        "audio/x-ms-asf"
        "audio/x-matroska"
        "audio/webm"
        "audio/vorbis"
        "audio/x-vorbis"
        "audio/x-shorten"
        "audio/x-ape"
        "audio/x-wavpack"
        "audio/x-tta"
        "audio/AMR"
        "audio/ac3"
        "audio/eac3"
        "audio/amr-wb"
        "audio/flac"
        "audio/mp4"
        "audio/x-pn-au"
        "audio/3gpp"
        "audio/3gpp2"
        "audio/dv"
        "audio/opus"
        "audio/vnd.dts"
        "audio/vnd.dts.hd"
        "audio/x-adpcm"
        "audio/m3u"
      ];

      videoTypes = [
        "video/x-ogm+ogg"
        "video/mpeg"
        "video/x-mpeg2"
        "video/x-mpeg3"
        "video/mp4v-es"
        "video/x-m4v"
        "video/mp4"
        "video/divx"
        "video/vnd.divx"
        "video/msvideo"
        "video/x-msvideo"
        "video/ogg"
        "video/quicktime"
        "video/vnd.rn-realvideo"
        "video/x-ms-afs"
        "video/x-ms-asf"
        "video/x-ms-wmv"
        "video/x-ms-wmx"
        "video/x-ms-wvxvideo"
        "video/x-avi"
        "video/avi"
        "video/x-flic"
        "video/fli"
        "video/x-flc"
        "video/flv"
        "video/x-flv"
        "video/x-theora"
        "video/x-theora+ogg"
        "video/x-matroska"
        "video/mkv"
        "video/webm"
        "video/x-ogm"
        "video/mp2t"
        "video/vnd.mpegurl"
        "video/3gp"
        "video/3gpp"
        "video/3gpp2"
        "video/dv"
      ];
    in
    (mkTypeAssocs audioTypes "mpv.desktop") // (mkTypeAssocs videoTypes "mpv.desktop");
}
