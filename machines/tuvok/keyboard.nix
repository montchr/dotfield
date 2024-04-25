{ lib, ... }:
let
  internal-keyboard-device-path = "/dev/input/by-path/platform-24eb30000.input-event-kbd";
in
{
  # By default, kanata captures all keyboard events, so most of the
  # configuration can be shared across every keyboard we use, by way of the
  # "default" configuration.  To prevent surprises, we specify devices explicitly.
  services.kanata.keyboards."default".devices = lib.singleton internal-keyboard-device-path;
}
