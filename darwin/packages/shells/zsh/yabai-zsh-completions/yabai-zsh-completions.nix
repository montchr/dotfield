{
  lib,
  stdenv,
  fetchFromGitHub,
  yabai,
  jq,
}:
stdenv.mkDerivation {
  pname = "yabai-zsh-completions";
  version = "unstable-2023-02-19";

  src = fetchFromGitHub {
    owner = "Amar1729";
    repo = "yabai-zsh-completions";
    rev = "07455f357db2c8ab683e43f0caa35f4ef0d75014";
    hash = "sha256-h0LhYWPzFGUtnc6FVjz1UG2vT7txiHb2PI/xYFyB3H4=";
  };

  buildInputs = [jq yabai];

  strictDeps = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/zsh/site-functions
    install -D --target-directory=$out/share/zsh/site-functions src/*

    runHook postInstall
  '';

  meta = with lib; {
    description = "zsh completions for the yabai tiling window manager";
    platforms = platforms.darwin;
    homepage = "https://github.com/Amar1729/yabai-zsh-completions";
    license = licenses.mit;
    maintainers = with maintainers; [montchr];
  };
}
