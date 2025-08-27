{ self, ... }:
let
  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.boschic = {
    system = "x86_64-linux";
    configuration = {
      imports = [
        ./configuration.nix

        (nixos + "/mixins/gnome.nix")
        (nixos + "/mixins/jobwork.nix")
        (nixos + "/mixins/workstation.nix")

        # FIXME: clarify that this means an amd cpu, NOT gpu
        (nixos + "/profiles/hardware/amd.nix")

        (nixos + "/profiles/hardware/focusrite-scarlett-18i20-mk1.nix")

        # TODO: rename to note that this is gpu, making it mutually exclusive
        #       with an AMD GPU (same goes for intel/amd cpu but i don't bother
        #       with intel cpus)
        (nixos + "/profiles/hardware/nvidia/stable-release.nix")
        (nixos + "/profiles/hardware/razer.nix")
      ];
    };
  };
}
