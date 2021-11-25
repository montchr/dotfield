{ stdenv, lib }:

let
  version = "0829";
in

stdenv.mkDerivation {
  name = "pragmatapro-${version}";
  version = version;

  src = builtins.fetchGit {
    # TODO: update url once fixed: https://github.com/NixOS/nix/issues/3503
    url = "ssh://git@github.com/montchr/pragmatapro.git";
    ref = "main";
    rev = "8965aa893e2290ee01880b1dc3b25747d0ebdf59";
  };

  phases = [ "unpackPhase" "installPhase" ];
  pathsToLink = [ "/share/fonts/opentype/" ];
  sourceRoot = ".";
  installPhase = ''
    install_path=$out/share/fonts/opentype
    mkdir -p $install_path
    find -name "PragmataPro*${version}*.otf" -exec cp {} $install_path \;
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
