{
  lib,
  stdenv,
  fetchFromGitHub,
}:

stdenv.mkDerivation rec {
  pname = "fzf-tab-completion";
  version = "unstable-2025-01-20";

  src = fetchFromGitHub {
    owner = "lincheney";
    repo = "fzf-tab-completion";
    rev = "4850357beac6f8e37b66bd78ccf90008ea3de40b";
    hash = "sha256-pgcrRRbZaLoChVPeOvw4jjdDCokUK1ew0Wfy42bXfQo=";
  };

  # NOTE: The readline integration is out of scope here because
  #       it is a Linux-only Rust package.
  installPhase = ''
    installPath=$out/share

    install -D bash/* -t $installPath/bash
    install -D node/* -t $installPath/node
    install -D zsh/* -t $installPath/zsh
  '';

  meta = {
    description = "Tab completion using fzf";
    homepage = "https://github.com/lincheney/fzf-tab-completion";
    license = lib.licenses.gpl3Only;
    maintainers = with lib.maintainers; [ montchr ];
    mainProgram = "fzf-tab-completion";
    platforms = lib.platforms.unix;
  };
}
