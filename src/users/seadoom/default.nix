flake@{ ... }:
let
  inherit (flake.config.dotfield) features meta;
  inherit (meta) users;
in
{
  dotfield.users.seadoom.baseline.features = [
    features.fish__with-ghostty-launch-command
  ];
}
