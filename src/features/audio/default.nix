{
  aspects.workstation.nixos =
    { pkgs, ... }:
    {
      # PulseAudio server uses this to acquire realtime priority.
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
        wireplumber.enable = true;
      };
    };
}
