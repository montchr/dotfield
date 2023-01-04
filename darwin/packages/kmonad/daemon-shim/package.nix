# Copyright (C) 2022 Matthew Toohey
# Source: <https://github.com/mtoohey31/infra/blob/e477bbf580358609a6c98721c403703d669fee41/pkgs/os-specific/darwin/kmonad-daemon-shim/default.nix>
{
  stdenv,
  lib,
}:
stdenv.mkDerivation {
  pname = "kmonad-daemon-shim";
  version = "0.1.0";
  src = lib.cleanSource ./.;
  buildPhase = ''
    cc main.c -o kmonad-daemon-shim
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp kmonad-daemon-shim $out/bin
  '';
}
