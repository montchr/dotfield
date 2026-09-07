# <https://wiki.nixos.org/wiki/Nvidia>

{ lib, ... }:
{
  aspects.hardware__nvidia = {
    nixos =
      { pkgs, config, ... }:
      {
        environment.systemPackages = [ pkgs.nvtopPackages.nvidia ];

        services.xserver.videoDrivers = [ "nvidia" ];

        hardware.graphics.enable = true;
        hardware.graphics.enable32Bit = true;
        hardware.nvidia = {
          # Required.
          modesetting.enable = true;

          # Prevent display corruption and/or application crashes upon
          # wake from a suspended or hibernated state.  Saves the entire
          # VRAM memory to /tmp/ instead of saving just the bare
          # essentials.
          powerManagement.enable = true;

          # Enable the Nvidia settings menu, accessible via `nvidia-settings`.
          nvidiaSettings = true;
        };

        programs.sway.extraOptions = [ "--unsupported-gpu" ];
      };

    home = {
      programs.mpv.config.hwdec = "vdpau";
    };
  };
}
