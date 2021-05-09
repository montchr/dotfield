{ stdenv, fetchgit, lib }:

let
  version = "0.828";
in
stdenv.mkDerivation {
  name = "pragmatapro-${version}";
  version = version;

  src = fetchgit {
    url = "git@github.com:montchr/pragmatapro.git";
    rev = "refs/tags/v${version}";
    sha256 = "sha256-BU8vPOQ6ZO1Z24wJELOS1/HrPBMKJX6N615VBmeOIgY=";
  };

  phases = [ "installPhase" ];
  pathsToLink = [ "/share/fonts/ttf/" ];
  sourceRoot = ".";
  installPhase = ''
    install_path=$out/share/fonts/ttf
    mkdir -p $install_path
    find -name "PragmataPro*.ttf" -exec cp {} $install_path \;
  '';

  meta = with lib; {
    homepage = "https://www.fsd.it/shop/fonts/pragmatapro/";
    description = ''
      PragmataPro™ is a condensed monospaced font optimized for screen,
      designed by Fabrizio Schiavi to be the ideal font for coding, math and engineering
    '';
    platforms = platforms.all;
    licence = licences.unfree;
  };
}
