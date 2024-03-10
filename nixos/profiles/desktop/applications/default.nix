{ pkgs, ... }:
{
  imports = [ ./firefox.nix ];

  environment.systemPackages = [
    # Provide a minimal and sensible default terminal emulator as a fallback
    # in case the desktop environment doesn't bundle its own application or
    # the user config doesn't specify one.
    pkgs.foot

    pkgs.signal-desktop
  ];
}
