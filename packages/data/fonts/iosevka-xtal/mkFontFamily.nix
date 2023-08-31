{
  iosevka,
  lib,
  fontWeights,
}: let
  inherit (builtins) replaceStrings;
  mkBuildPlan = import ./mkBuildPlan.nix {inherit lib fontWeights;};
in
  args: let
    privateBuildPlan = mkBuildPlan args;
    set = lib.pipe privateBuildPlan.family [
      (replaceStrings ["Iosevka "] [""])
      (replaceStrings [" "] ["-"])
      lib.toLower
    ];
  in
    iosevka.override {
      inherit set privateBuildPlan;
    }
