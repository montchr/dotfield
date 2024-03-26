{ lib, ... }:
let
  internal-keyboard-device-path = "/dev/input/by-path/platform-24eb30000.input-event-kbd";
in
{
  imports = [
    ./users/cdom.nix
    ./secrets/sops.nix
    ./hardware-configuration.nix
  ];

  time.timeZone = "America/New_York";

  users.mutableUsers = false;

  dotfield.guardian.enable = true;
  dotfield.guardian.username = "cdom";

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

  # Not yet: <https://asahilinux.org/2024/01/fedora-asahi-new/#can-we-run-steam>
  # programs.steam.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
