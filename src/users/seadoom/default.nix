{ config, ... }:
let
  inherit (config.dotfield) aspects meta;
in
{
  dotfield.users.seadoom = {
    baseline = {
      aspects = [
        aspects.fish__with-ghostty-launch-command
      ];
    };
  };

  dotfield.meta.users.seadoom = meta.users.cdom;
}
