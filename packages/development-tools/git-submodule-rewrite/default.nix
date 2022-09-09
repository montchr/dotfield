{
  stdenv,
  gitignoreSource,
}:
stdenv.mkDerivation rec {
  name = "git-submodule-rewrite";
  src = gitignoreSource ./git-submodule-rewrite;
  installPhase = ''
    mkdir -p $out/bin
    cp bin/* $out/bin/
  '';
}
