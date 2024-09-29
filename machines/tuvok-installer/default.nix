{ lib, ... }:
{
  hardware.asahi.useExperimentalGPUDriver = true;
  hardware.asahi.experimentalGPUInstallMode = "overlay";

  # Match settings from nixos-apple-silicon
  boot.loader.systemd-boot.consoleMode = lib.mkForce "0";

  # Match other settings
  programs.ssh.setXAuthLocation = lib.mkForce true;

}
