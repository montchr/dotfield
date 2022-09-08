{
  stdenv,
  gitignoreSource,
}:
stdenv.mkDerivation {
  name = "ediff-tool";
  src = gitignoreSource ./ediff-tool;
  installPhase = ''
    mkdir -p $out/bin
    cp bin/* $out/bin/
  '';
}
