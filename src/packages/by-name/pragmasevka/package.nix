{
  lib,
  iosevka,
  fetchFromGitHub,
  buildNpmPackage,
  python3,
}:

let
  pname = "pragmasevka";
  version = "1.7.0";

  src = fetchFromGitHub {
    owner = "shytikov";
    repo = "pragmasevka";
    rev = "v${version}";
    hash = "sha256-r8TJapdnjNQTBDwCnxbMQhVccqKslgfhAHhFp1ohBSc=";
  };

  buildNpmPackage' =
    args:
    buildNpmPackage (
      args
      // {
        inherit pname version;
        src = fetchFromGitHub {
          owner = "be5invis";
          repo = "iosevka";
          rev = "v32.5.0";
          hash = "sha256-MzsAkq5l4TP19UJNPW/8hvIqsJd94pADrrv8wLG6NMQ=";
        };
        npmDepsHash = "sha256-HeqwpZyHLHdMhd/UfXVBonMu+PhStrLCxAMuP/KuTT8=";
      }
    );
in

(iosevka.override {
  privateBuildPlan = src.outPath + "/private-build-plans.toml";
  set = pname;
  buildNpmPackage = buildNpmPackage';
}).overrideAttrs
  (prevAttrs: {
    nativeBuildInputs = prevAttrs.nativeBuildInputs ++ [
      (python3.withPackages (
        pp: with pp; [
          fontforge
        ]
      ))
    ];

    # FIXME: without the fontforge modifications, it is not a true
    # packaging for the upstream font.  we have only built some
    # incomplete version of the font.
    #
    postBuild = (prevAttrs.postBuild or "") + ''
      cp ${src.outPath}/punctuation.py ./
      python ./punctuation.py ./pragmasevka
    '';

    meta = {
      description = "Pragmata Pro doppelgänger made of Iosevka SS08";
      homepage = "https://github.com/shytikov/pragmasevka";
      license = lib.licenses.ofl;
      maintainers = with lib.maintainers; [ montchr ];
      mainProgram = "pragmasevka";
      platforms = lib.platforms.all;
    };
  })
