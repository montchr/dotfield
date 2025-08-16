flake@{ ... }:
let
  inherit (flake.config.dotfield) features;
in
{
  dotfield.users.cdom.features = [
    features.fish__with-ghostty-launch-command
  ];
}
