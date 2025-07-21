{
  flake.modules.nixos.audio =
    { pkgs, ... }:
    {
      # According to its description in the NixOS manual:
      # > PulseAudio server uses this to acquire realtime priority.
      security.rtkit.enable = true;

      environment.systemPackages = [
        pkgs.pavucontrol
        pkgs.pwvucontrol
      ];

      services.pulseaudio.enable = false;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        jack.enable = true;
        pulse.enable = true;
        # "Modular session / policy manager for PipeWire"
        wireplumber.enable = true;
      };
    };

  flake.modules.nixos.admin-user = {
    # <https://wiki.archlinux.org/title/Users_and_groups#Pre-systemd_groups>
    dotfield.guardian.extraGroups = [
      "audio"
    ];
  };
}
