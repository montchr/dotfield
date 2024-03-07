{
  stdenv,
  requireFile,
  unzip,
  lib,
}:
stdenv.mkDerivation {
  pname = "berkeley-mono";
  version = "1.009";

  src = requireFile rec {
    name = "berkeley-mono-typeface.zip";
    url = "https://berkeleygraphics.com/typefaces/berkeley-mono/";
    sha256 = "17cqpql8zvakczvjhbzp6mgxvr137av2nik53p0ylk6gwjlqklv1";
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
    homepage = "https://berkeleygraphics.com/typefaces/berkeley-mono/";
    description = ''
      A love letter to the golden era of computing.
    '';
    platforms = platforms.all;
    licence = licences.unfree;
  };
}
