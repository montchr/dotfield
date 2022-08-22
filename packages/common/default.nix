final: prev: {
  kitty-helpers = final.lib.recurseIntoAttrs (final.callPackage ./kitty-helpers.nix {});

  ##: dotfield internals -------------------------------------------------------

  dotfield-config = final.stdenv.mkDerivation {
    name = "dotfield-config";
    # FIXME: what the...
    src = final.gitignoreSource ../../home/users/seadoom/config;
    installPhase = ''
      mkdir -p $out
      cp -R * $out/
    '';
  };

  ##: third-party scripts ------------------------------------------------------
}
