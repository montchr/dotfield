{
  stdenv,
  gitignoreSource,
}:
stdenv.mkDerivation rec {
  name = "git-submodule-rewrite";
  src = gitignoreSource ./.;
  installPhase = ''
    mkdir -p $out/bin
    cp bin/* $out/bin/
  '';
}
