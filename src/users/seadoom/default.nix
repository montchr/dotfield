{ config, ... }:
let
  inherit (config.dotfield) features meta;
in
{
  dotfield.users.seadoom = {
    baseline = {
      features = [
        features.fish__with-ghostty-launch-command
      ];
    };
  };

  dotfield.meta.users.seadoom = meta.users.cdom;
}
