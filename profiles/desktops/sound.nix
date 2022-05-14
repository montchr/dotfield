{
  config,
  lib,
  pkgs,
  ...
}: {
  sound.enable = true;
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    pulse.enable = true;
  };
  hardware.pulseaudio.enable = false;

  environment.systemPackages = with pkgs; [
    cmus
    pavucontrol
    playerctl
  ];
}
