{
  stdenv,
  gitignoreSource,
}:
stdenv.mkDerivation {
  name = "dotfield-config";
  # FIXME: what the...
  src = gitignoreSource ../../home/users/seadoom/config;
  installPhase = ''
    mkdir -p $out
    cp -R * $out/
  '';
}
