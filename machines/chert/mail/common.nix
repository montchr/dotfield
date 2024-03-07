{ pkgs, ... }:
{
  # Include common mail utilities for testing, since this is a mailserver.
  environment.systemPackages = [
    pkgs.aerc
    pkgs.isync
    pkgs.mu
    pkgs.mutt
  ];
}
