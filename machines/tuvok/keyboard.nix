{ lib, ... }:
let
  internal-keyboard-device-path = "/dev/input/by-path/platform-24eb30000.input-event-kbd";
in
{
  dotfield.hardware.keyboard.remapping.provider = "kanata";

  # By default, kanata captures all keyboard events, so most of the
  # configuration can be shared across every keyboard we use, by way of the
  # "default" configuration.  To prevent surprises, we specify devices explicitly.
  services.kanata.keyboards."default".devices = lib.singleton internal-keyboard-device-path;

  # The following configuration for this virtual keyboard should be specific to
  # the internal Apple keyboard.
  # NOTE: Disabled because only one should be in use at a time...
  # services.kanata.keyboards."apple-mtp-keyboard".devices = lib.singleton internal-keyboard-device-path;

  # The KMonad module, in contrast to the Kanata module above, always needs the
  # device.  This is a technical detail of KMonad.
  services.kmonad.keyboards."default".device = internal-keyboard-device-path;
}
