{
  lib,
  iosevka,
  fetchFromGitHub,
}:

let
  sets = [
    "sans"
    "sans-mono"
    "serif"
    "serif-mono"
  ];
  version = "1.0.0";
  src = fetchFromGitHub {
    owner = "protesilaos";
    repo = "aporetic";
    rev = version;
    sha256 = "sha256-5m4iT77FFsJf6N1FIsCtk5Z0IEOVUGCceXiT2n5dZUg=";
  };
  privateBuildPlan = src.outPath + "/private-build-plans.toml";
in
builtins.listToAttrs (
  builtins.map (set: {
    name = set;
    value =
      (iosevka.override {
        inherit set privateBuildPlan;
      }).overrideAttrs
        {
          inherit version;
          pname = "aporetic-${set}";
          meta = with lib; {
            inherit (src.meta) homepage;
            description = ''
              Customised build of the Iosevka typeface, with a consistent
              rounded style and overrides for almost all individual glyphs
              in both roman (upright) and italic (slanted) variants.
            '';
            license = licenses.ofl;
            platforms = iosevka.meta.platforms;
            maintainers = [ maintainers.DamienCassou ];
          };
        };
  }) sets
)
