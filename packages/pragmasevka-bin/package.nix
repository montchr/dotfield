{
  stdenv,
  lib,
  fetchurl,
  unzip,
}:
let
  name = "pragmasevka";
  version = "1.7.0";
in
stdenv.mkDerivation {
  inherit version;
  pname = "${name}-bin";

  src = fetchurl {
    url = "https://github.com/shytikov/pragmasevka/releases/download/v${version}/Pragmasevka.zip";
    sha256 = "sha256-fgOeGkgmINxGmcQe4iS36c9ON/2KZqMhh6YOKp93noE=";
  };

  nativeBuildInputs = [ unzip ];

  dontInstall = true;

  unpackPhase = ''
    mkdir -p $out/share/fonts
    unzip -d $out/share/fonts/truetype $src
  '';

  meta = {
    description = "Pragmata Pro doppelgänger made of Iosevka SS08";
    homepage = "https://github.com/shytikov/pragmasevka";
    license = lib.licenses.ofl;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "pragmasevka";
    platforms = lib.platforms.all;
  };

  # FIXME: not yet complete
  # passthru.updateScript = ./update-bin.sh;
}
