{
  lib,
  fetchFromGitHub,
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "firefox-ui-fix";
  version = "7.3.1";

  src = fetchFromGitHub {
    owner = "black7375";
    repo = "Firefox-UI-Fix";
    rev = "v${version}";
    sha256 = "sha256-zyW/nZPoee/S0f4KLH1sveeYjuWJetlChpwCfeSXomM=";
  };

  installPhase = ''
    mkdir -p $out/chrome
    cp -t $out/ CREDITS LICENSE user.js
    cp -R ./{css,icons} $out/chrome/
  '';

  meta = with lib; {
    # FIXME: come up with a decent description because the project doesn't seem
    # to provide anything usable...
    # description = ''
    # '';
    license = with licenses; [mpl20];
    maintainers = with maintainers; [montchr];
  };
}
