{
  config,
  lib,
  modulesPath,
  ...
}:
{
  disabledModules = [ "virtualisation/parallels-guest.nix" ];

  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    ./parallels-guest.nix
  ];

  # FIXME: must be set when insantiating nixpkgs
  # nixpkgs.config.allowUnfreePredicate = pkg:
  #   builtins.elem (lib.getName pkg) [
  #     "prl-tools"
  #   ];

  hardware.parallels = {
    enable = true;
    package = config.boot.kernelPackages.callPackage ./prl-tools.nix { };
  };

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
