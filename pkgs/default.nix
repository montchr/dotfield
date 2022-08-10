final: prev: {
  sources = prev.callPackage (import ./_sources/generated.nix) {};

  kitty-helpers = final.lib.recurseIntoAttrs (final.callPackage ./kitty-helpers.nix {});

  ## dotfield internals ========================================================

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

  ## third-party scripts =======================================================

  ediff-tool = final.stdenv.mkDerivation rec {
    name = "ediff-tool";
    src = final.gitignoreSource ../vendor/ediff-tool;
    installPhase = ''
      mkdir -p $out/bin
      cp ${name} $out/bin/${name}
    '';
  };

  git-submodule-rewrite = final.stdenv.mkDerivation rec {
    name = "git-submodule-rewrite";
    src = final.gitignoreSource ../vendor/git-submodule-rewrite;
    installPhase = ''
      mkdir -p $out/bin
      cp bin/${name} $out/bin/${name}
    '';
  };

  ## fonts =====================================================================

  nerdfonts-symbols-only = final.callPackage ./fonts/nerdfonts-symbols-only.nix {};
  sf-pro = final.callPackage ./fonts/sf-pro.nix {};

  ##: promnesia ----------------------------------------------------------------

  promnesia = final.callPackage ./python/promnesia {
    inherit (final) hpi orgparse;
  };

  # dependencies
  hpi = final.callPackage ./python/HPI {};
  orgparse = final.callPackage ./python/orgparse {};
}
