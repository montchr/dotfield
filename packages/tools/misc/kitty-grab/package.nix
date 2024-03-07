{
  lib,
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  # TODO: what's the norm re: packages whose real name includes an underscore?
  pname = "kitty-grab";
  version = "0.20";

  src = fetchFromGitHub {
    owner = "yurikhan";
    repo = "kitty_grab";
    rev = "v${version}";
    hash = "sha256-B6HRp2cPRAo9DFFl8QWsXvZNxn42K5FCzqJDDtaDVKE=";
  };

  # FIXME: will not include hidden files
  installPhase = ''
    mkdir -p $out
    cp * $out/
  '';

  meta = with lib; {
    description = "Keyboard-driven screen grabber for Kitty";
    homepage = "https://github.com/yurikhan/kitty_grab";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ montchr ];
    mainProgram = "kitty-grab";
    platforms = platforms.all;
  };
}
