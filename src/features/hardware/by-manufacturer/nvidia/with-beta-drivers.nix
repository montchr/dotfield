{ lib, ... }:
{
  dotfield.modules."hardware/nvidia/with-beta-drivers".nixos =
    { config, ... }:
    let
      inherit (config.boot.kernelPackages) nvidiaPackages;
    in
    {
      hardware.nvidia.package =
        if (lib.versionOlder nvidiaPackages.beta.version nvidiaPackages.stable.version) then
          nvidiaPackages.stable
        else
          nvidiaPackages.beta;
    };
}
