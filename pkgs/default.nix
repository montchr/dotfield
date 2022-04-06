final: prev: {
  # keep sources this first
  sources = prev.callPackage (import ./_sources/generated.nix) {};
  # then, call packages with `final.callPackage`

  dotfield = prev.callPackage (import ./dotfield.nix) {};
  dotfield-config = prev.stdenv.mkDerivation {
    name = "dotfield-config";
    src = final.gitignoreSource ../config;
    installPhase = ''
      mkdir -p $out
      cp -R * $out/
    '';
  };
  dotfield-vendor = prev.stdenv.mkDerivation {
    name = "dotfield-vendor";
    src = final.gitignoreSource ../vendor;
    installPhase = ''
      mkdir -p $out
      cp -R .bin $out/.bin
    '';
  };
}
