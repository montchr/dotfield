{
  dotfield.features.workstation.nixos =
    { pkgs, ... }:
    {
      hardware.bluetooth.enable = true;
      hardware.bluetooth.package = pkgs.bluez;
      hardware.bluetooth.powerOnBoot = true;
      hardware.bluetooth.settings = {
        General = {
          # Bluetooth headsets will have unusable amounts of choppy audio without
          # this. Why "Headset" needs to be in "Disable" is counterintuitive for me, at least.
          # TODO: consider upstreaming to nixos as a new option?
          # <https://askubuntu.com/questions/863930/bluetooth-headset-cant-set-a2dp-high-fidelity-playback-poor-sound-quality>
          # <https://wiki.archlinux.org/title/Bluetooth_headset#A2DP_not_working_with_PulseAudio>
          Disable = "Headset";
        };
      };
    };
}
