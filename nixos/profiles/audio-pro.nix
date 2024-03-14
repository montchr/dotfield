# <https://discourse.nixos.org/t/usb-audio-interface-not-recognized/35159>
# <https://wiki.archlinux.org/index.php/Pro_Audio>
# <https://nixos.wiki/wiki/PipeWire>
# possible conflict with musnix rt kernel + nvidia proprietary driver: <https://github.com/musnix/musnix/issues/127>
{ flake, ... }:
{
  imports = [
    ./audio.nix

    flake.inputs.musnix.nixosModules.musnix
  ];

  environment.systemPackages = [ ];

  # Optimizations for real-time audio support.
  #
  # Note that this does not automatically enable the realtime kernel patch, as
  # that would require rebuilding the kernel.
  #
  # <https://github.com/musnix/musnix?tab=readme-ov-file#kernel-options>
  musnix.enable = true;
}
