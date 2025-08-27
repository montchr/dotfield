{ self, ... }:
let
  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.hodgepodge = {
    system = "x86_64-linux";
    configuration = {
      imports = [
        ./configuration.nix

        (nixos + "/mixins/gnome.nix")
        (nixos + "/mixins/workstation.nix")

        (nixos + "/profiles/hardware/apple/macbookpro-11-3.nix")
      ];
    };
  };
}
