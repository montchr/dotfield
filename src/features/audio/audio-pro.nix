### Professional-grade audio production

# <https://discourse.nixos.org/t/usb-audio-interface-not-recognized/35159>
# <https://wiki.archlinux.org/index.php/Pro_Audio>
# <https://wiki.nixos.org/wiki/PipeWire>

# possible conflict with musnix rt kernel + nvidia proprietary driver:
# <https://github.com/musnix/musnix/issues/127>

{ inputs, ... }:
{
  dotfield.features."audio/pro".nixos = {
    imports = [
      inputs.musnix.nixosModules.musnix
    ];

    environment.systemPackages = [ ];

    musnix.enable = true;
    musnix.rtcqs.enable = true;
    musnix.das_watchdog.enable = true;
  };
}
