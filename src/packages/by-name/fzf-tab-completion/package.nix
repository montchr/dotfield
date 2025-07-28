{
  lib,
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation rec {
  pname = "fzf-tab-completion";
  version = "unstable-2023-11-09";

  src = fetchFromGitHub {
    owner = "lincheney";
    repo = "fzf-tab-completion";
    rev = "f6f83c88eca0fc07f7820dd8bb6c7ea75ef478c5";
    hash = "sha256-AWgf8jSticYgO+qzTc/YjO1dZrh1fqSJPbgqyu/oLxE=";
  };

  # NOTE: The readline integration is out of scope here because
  #       it is a Linux-only Rust package.
  installPhase = ''
    installPath=$out/share

    install -D bash/* -t $installPath/bash
    install -D node/* -t $installPath/node
    install -D zsh/* -t $installPath/zsh
  '';

  meta = with lib; {
    description = "Tab completion using fzf";
    homepage = "https://github.com/lincheney/fzf-tab-completion";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ montchr ];
    mainProgram = "fzf-tab-completion";
    platforms = platforms.all;
  };
}
