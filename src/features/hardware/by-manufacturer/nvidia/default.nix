# <https://wiki.nixos.org/wiki/Nvidia>

{ lib, ... }:
{
  aspects.hardware__nvidia =
    { config, ... }:
    let
      inherit (config.boot.kernelPackages) nvidiaPackages;
    in
    {
      nixos = {
        services.xserver.videoDrivers = [ "nvidia" ];

        hardware.graphics.enable = true;
        # Whether to support direct rendering for 32-bit applications (e.g. Wine).
        hardware.graphics.enable32Bit = true;
        hardware.nvidia = {
          # Required.
          modesetting.enable = true;

          package =
            if (lib.versionOlder (nvidiaPackages.stable).version (nvidiaPackages.beta).version) then
              nvidiaPackages.beta
            else
              nvidiaPackages.stable;

          # Prevent display corruption and/or application crashes upon wake from a
          # suspended or hibernated state.  Saves the entire VRAM memory to /tmp/
          # instead of saving just the bare essentials.
          #
          # NOTE: This is an experimental setting.  It can cause sleep/suspend to
          # fail.  Consider avoiding.
          powerManagement.enable = true;
          # Fine-grained power management.  Turns off GPU when not in use.
          # NOTE: Experimental and only works on modern Nvidia GPUs (Turing or newer).
          powerManagement.finegrained = false;

          # Enable the Nvidia settings menu, accessible via `nvidia-settings`.
          nvidiaSettings = true;

          # Whether to use the Nvidia open source kernel module (not to be confused
          # with the independent third-party "nouveau" open source driver).
          #
          # Support is limited to the Turing and later architectures:
          # <https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus>
          #
          # XXX: Currently alpha-quality/buggy, so false is currently the setting
          # recommended by NixOS documentation.
          open = false;
        };

        programs.sway.extraOptions = [ "--unsupported-gpu" ];
      };

      home = {
        programs.mpv.config.hwdec = "vdpau";
      };
    };
}
