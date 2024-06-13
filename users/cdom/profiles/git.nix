{ flake, pkgs, ... }:
let
  inherit (flake) packages;
in
{
  programs.gh.extensions = [
    packages.gh-i
    packages.gh-repo-explore
    packages.gh-s

    pkgs.gitAndTools.tig

    pkgs.gh-dash
    pkgs.gh-eco
  ];
  programs.lazygit.enable = true;

  programs.git.extraConfig = {
    tig.line-graphics = "auto";
    pull.rebase = true;
    merge.tool = "ediff";
    diff = {
      algorithm = "histogram";
      exif.textconv = "${pkgs.exiftool}/bin/exiftool";
      colorMoved = "dimmed-zebra";
      tool = "ediff";
    };
  };
}
