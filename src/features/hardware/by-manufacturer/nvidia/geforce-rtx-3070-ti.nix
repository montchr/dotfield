{ inputs, ... }: {
  aspects.hardware__nvidia__geforce-rtx-3070-ti = {
    requires = [ "hardware__nvidia" ];
    nixos = {
      imports = [
        # This is referenced "indirectly" through its path, rather than
        # through `inputs.nixos-hardware.nixosModules` because of
        # <https://github.com/NixOS/nixos-hardware/issues/992>.
        #
        # TODO: Fix this when <https://github.com/NixOS/nixos-hardware/issues/992> gets merged.
        "${inputs.nixos-hardware}/common/gpu/nvidia/ampere"
      ];
    };
  };
}
