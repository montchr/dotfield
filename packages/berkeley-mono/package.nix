{
  stdenv,
  requireFile,
  unzip,
  lib,
}:
stdenv.mkDerivation {
  pname = "berkeley-mono";
  version = "2.002";

  src = requireFile rec {
    name = "berkeley-mono-typeface.zip";
    url = "https://usgraphics.com/products/berkeley-mono";
    sha256 = "15imcrymcby2jja9fh9mz7z0lf0ajvi1nv5dvswv7rmgn8gfjf8b";
    message = ''
      ${name} font not found in nix store, to add it run:
      $ nix-store --add-fixed sha256 /path/to/${name}

      Did you change the file? maybe you need to update the sha256
      $ nix-hash --flat --base32 --type sha256 /path/to/${name}

      More info: <https://nixos.org/manual/nixpkgs/unstable/#requirefile>
    '';
  };

  buildInputs = [ unzip ];
  phases = [
    "unpackPhase"
    "installPhase"
  ];
  pathsToLink = [ "/share/fonts/truetype/" ];
  sourceRoot = ".";
  installPhase = ''
    install_path=$out/share/fonts/truetype
    mkdir -p $install_path
    find -name "BerkeleyMono*.ttf" -exec cp {} $install_path \;
  '';

  meta = with lib; {
    homepage = "https://usgraphics.com/products/berkeley-mono";
    description = ''
      Berkeley Mono™ is a love letter to the golden era of computing.
      The era that gave rise to a generation of people who celebrated
      automation and reveled in the joy of computing, when transistors
      replaced cogs, and machine-readable typefaces were developed,
      for when humans and machines truly interfaced on an unprecedented scale.
    '';
    platforms = platforms.all;
    licence = licences.unfree;
  };
}
