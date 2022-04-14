final: prev: {
  # keep sources this first
  sources = prev.callPackage (import ./_sources/generated.nix) {};
  # then, call packages with `final.callPackage`

  ## dotfield internals ========================================================

  dotfield = prev.callPackage (import ./dotfield.nix) {};

  dotfield-config = prev.stdenv.mkDerivation {
    name = "dotfield-config";
    src = final.gitignoreSource ../config;
    installPhase = ''
      mkdir -p $out
      cp -R * $out/
    '';
  };

  # dotfield-php = prev.stdenv.mkDerivation {
  #   name = "dotfield-php";
  #   src = ./dotfield-php/vendor/bin;
  #   installPhase = ''
  #     mkdir -p $out
  #     cp -L * $out/bin
  #   '';
  # };

  dotfield-vendor = prev.stdenv.mkDerivation {
    name = "dotfield-vendor";
    src = final.gitignoreSource ../vendor;
    installPhase = ''
      mkdir -p $out
      cp -R .bin $out/.bin
    '';
  };


  ## fonts =====================================================================

  pragmatapro = final.callPackage ./fonts/pragmatapro.nix {};
  sf-pro = final.callPackage ./fonts/sf-pro.nix {};
}
