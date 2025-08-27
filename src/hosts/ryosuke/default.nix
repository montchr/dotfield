{ self, inputs, ... }:
let
  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.ryosuke = {
    system = "x86_64-linux";
    configuration = {
      imports = [
        ./configuration.nix

        inputs.nixos-hardware.nixosModules.common-cpu-amd
        inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
        inputs.nixos-hardware.nixosModules.common-gpu-amd

        (nixos + "/mixins/gnome.nix")
        (nixos + "/mixins/jobwork.nix")
        (nixos + "/mixins/workstation.nix")

        (nixos + "/profiles/hardware/razer.nix")
        (nixos + "/profiles/remote-builders/default.nix")
      ];
    };
  };
}
