{
  lib,
  iosevka,
  fetchFromGitHub,
  buildNpmPackage,
  python3,
}:

let
  pname = "astrata-mono";
  version = "1.0.0";

  src = lib.cleanSource ./.;

  buildNpmPackage' =
    args:
    buildNpmPackage (
      args
      // {
        inherit pname version;
        src = fetchFromGitHub {
          owner = "be5invis";
          repo = "iosevka";
          rev = "v33.2.7";
          hash = "sha256-LS4c79/QmBSgsgSY/Tddmf2M6U0LcISMapnWK7Eu1Ak=";
        };
        npmDepsHash = "sha256-DyExvgNJBRlz8iVezlrJfTyobK0L6CMQN+gIMGoYYrw=";
      }
    );
in

(iosevka.override {
  privateBuildPlan = src.outPath + "/private-build-plans.toml";
  set = pname;
  buildNpmPackage = buildNpmPackage';
}).overrideAttrs
  (_prevAttrs: {
    meta = {
      description = "A custom Iosevka build by @astratagem";
      homepage = "https://codeberg.org/astratagem/dotfield";
      license = lib.licenses.ofl;
      maintainers = with lib.maintainers; [ montchr ];
      mainProgram = "astrata-mono";
      platforms = lib.platforms.all;
    };
  })
