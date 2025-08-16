flake@{ ... }:
let
  inherit (flake.config.dotfield) features meta;
  inherit (meta) users;
in
{
  dotfield.users.seadoom.features = [
    features.fish__with-ghostty-launch-command
  ];

  dotfield.meta.users.seadoom = { inherit (users.cdom) keys preferences whoami; };
}
