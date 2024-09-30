# NOTE: Enabling this will rebuild the world!  But holy shit, it is worth it.
{ pkgs, ... }:
{
  hardware.asahi.useExperimentalGPUDriver = true;
  hardware.asahi.experimentalGPUInstallMode = "overlay";

  # Reduce frequency of expensive builds.
  # TODO: needs nixos-unstable update (which currently fails due to asahi
  # overlay issue with mesa override package)
  # programs.firefox.package = pkgs.firefox-esr-128;
}
