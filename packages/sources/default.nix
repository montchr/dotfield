final: prev: {
  __dontExport = true;

  # nvfetcher sources
  sources = prev.callPackage ./_sources/generated.nix {};

  ediff-tool = final.stdenv.mkDerivation rec {
    name = "ediff-tool";
    src = final.gitignoreSource ./ediff-tool;
    installPhase = ''
      mkdir -p $out/bin
      cp bin/* $out/bin/
    '';
  };

  git-submodule-rewrite = final.stdenv.mkDerivation rec {
    name = "git-submodule-rewrite";
    src = final.gitignoreSource ./git-submodule-rewrite;
    installPhase = ''
      mkdir -p $out/bin
      cp bin/* $out/bin/
    '';
  };
}
