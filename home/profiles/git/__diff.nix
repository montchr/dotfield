{ pkgs, ... }:
{
  home.packages = [
    pkgs.difftastic # <- syntax-aware structural diff tool
    pkgs.exiftool # <- EXIF diff handler
  ];

  programs.git.extraConfig = {
    rerere.enabled = true;
    merge.conflictstyle = "diff3";
    merge.tool = "ediff";

    diff = {
      algorithm = "histogram";
      exif.textconv = "${pkgs.exiftool}/bin/exiftool";
      colorMoved = "dimmed-zebra";
      tool = "ediff";
    };
  };
}
