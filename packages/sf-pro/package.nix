{
  stdenv,
  lib,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  name = "sf-pro";

  # <https://github.com/sahibjotsaggu/San-Francisco-Pro-Fonts>
  src = fetchFromGitHub {
    owner = "sahibjotsaggu";
    repo = "San-Francisco-Pro-Fonts";
    rev = "8bfea09aa6f1139479f80358b2e1e5c6dc991a58";
    hash = "sha256-mAXExj8n8gFHq19HfGy4UOJYKVGPYgarGd/04kUIqX4=";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];
  pathsToLink = [ "/share/fonts/opentype/" ];
  sourceRoot = ".";
  installPhase = ''
    install_path=$out/share/fonts/opentype
    mkdir -p $install_path
    cp **/*.{otf,ttf} $install_path
  '';

  meta = with lib; {
    homepage = "https://developer.apple.com/fonts/";
    description = ''
      This neutral, flexible, sans-serif typeface is the system font for iOS,
      iPad OS, macOS and tvOS. SF Pro features nine weights, variable optical
      sizes for optimal legibility, and includes a rounded variant. SF Pro
      supports over 150 languages across Latin, Greek, and Cyrillic scripts.
    '';
    platforms = platforms.all;
    license = licenses.unfree;
  };
}
