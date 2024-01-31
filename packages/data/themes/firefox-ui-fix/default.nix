{
  lib,
  fetchFromGitHub,
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "firefox-ui-fix";
  version = "unstable-2024-01-29";

  src = fetchFromGitHub {
    owner = "black7375";
    repo = "Firefox-UI-Fix";
    rev = "ed315431274ac367f89b7934b360ba2692cf544f";
    sha256 = "sha256-VEPckzCEGIrewsBaYClqPgJcYedHPNHP3uwBI2nSmzM=";
  };

  installPhase = ''
    mkdir -p $out/chrome
    cp -t $out/ CREDITS LICENSE user.js
    cp -R ./{css,icons} $out/chrome/
  '';

  meta = with lib; {
    # TODO: project does not have a defined description anywhere
    # description = ''
    # '';
    license = with licenses; [mpl20];
    maintainers = with maintainers; [montchr];
  };
}
