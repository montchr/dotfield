{ lib, flake, ... }:
{
  imports = [
    ../../boot/systemd-boot.nix
  ];

  boot.loader.systemd-boot.consoleMode = lib.mkForce "0";
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  # Mutually exclusive legacy Apple hardware.
  hardware.facetimehd.enable = lib.mkForce false;
  services.mbpfan.enable = lib.mkForce false;

  hardware.asahi.enable = true;
  hardware.asahi.useExperimentalGPUDriver = true;

  # Recent GTK uses Vulkan as a backend.  On Asahi the OpenGL driver
  # seems to perform better than the Vulkan part, so we fall back to
  # OpenGL for GTK for now.
  #
  # <https://github.com/oliverbestmann/nixos-apple-silicon/blob/434afde300c2bcaf3cf8dc068c0272b6230eb154/apple-silicon-support/modules/default.nix#L16-L21>
  environment.variables = {
    GSK_RENDERER = "ngl";
  };
}
