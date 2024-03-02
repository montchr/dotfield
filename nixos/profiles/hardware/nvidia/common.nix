# <https://nixos.wiki/wiki/Nvidia#Modifying_NixOS_Configuration>
{
  lib,
  pkgs,
  ...
}: {
  services.xserver.videoDrivers = ["nvidia"];

  # TODO: remind self about nature of vaapiVdpau (related to video decoding, but
  # why this package?) -- this may very likely relate to issues with Steam
  # Remote Play encountered whilst trying to run Baldur's Gate 3 on ryosuke as
  # client machine (host was DORE (mswin), which is the same machine as boschic,
  # and thus nvidia)...
  hardware.opengl.extraPackages = with pkgs; [vaapiVdpau];

  # Required for Wayland support.
  hardware.nvidia.modesetting.enable = true;

  # Prevent display corruption upon wake from a suspended or hibernated state.
  # FIXME(docs): provide evidence/source
  # Probably does other important things too. We don't want the GPU to break...
  # NOTE: This is an experimental setting. Consider avoiding.
  hardware.nvidia.powerManagement.enable = lib.mkDefault true;

  # Enable the Nvidia settings menu,
  # accessible via `nvidia-settings`.
  hardware.nvidia.nvidiaSettings = true;

  hardware.opengl = {
    enable = true;
    driSupport = true;
    # "On 64-bit systems, whether to support Direct Rendering for 32-bit
    # applications (such as Wine). This is currently only supported for the
    # nvidia as well as Mesa."
    driSupport32Bit = true;
  };

  # Whether to use the Nvidia open source kernel module (not to be confused
  # with the independent third-party "nouveau" open source driver).

  # Support is limited to the Turing and later architectures. Full list of
  # supported GPUs is at:
  # <https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus>

  # Currently alpha-quality/buggy, so false is currently the recommended setting.
  hardware.nvidia.open = false;
}
