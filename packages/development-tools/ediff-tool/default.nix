{
  stdenv,
  gitignoreSource,
}:
stdenv.mkDerivation {
  name = "ediff-tool";
  src = gitignoreSource ./.;
  installPhase = ''
    mkdir -p $out/bin
    cp bin/* $out/bin/
  '';
}
