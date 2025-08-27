{ self, ... }:
let
  nixos = self.outPath + "/nixos";
in
{
  hosts.nixos.tuuvok = {
    system = "aarch64-linux";
    channel = "nixpkgs-apple-silicon";
    configuration = {
      imports = [
        ./configuration.nix

        (nixos + "/mixins/jobwork.nix")
        # ./mixins/gnome.nix
        (nixos + "/mixins/sway.nix")
        (nixos + "/mixins/workstation.nix")

        (nixos + "/profiles/hardware/apple/macbook-14-2/default.nix")
        (nixos + "/profiles/hardware/displaylink.nix")

        (nixos + "/profiles/remote-builders/default.nix")
        (nixos + "/profiles/remote-builders/ryosuke.nix")
      ];
    };
  };
}
