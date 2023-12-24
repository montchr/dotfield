{
  lib,
  fetchFromGitHub,
  stdenv,
}:
stdenv.mkDerivation rec {
  pname = "firefox-ui-fix";
  version = "8.5.1";

  src = fetchFromGitHub {
    owner = "black7375";
    repo = "Firefox-UI-Fix";
    rev = "v${version}";
    sha256 = "sha256-qiJmA25HVw/IOQictcnhTxXfbUWQ11TTEcQTEzIeMug=";
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
