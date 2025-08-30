{ config, self, ... }:
let
  inherit (config.meta) keys;
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

  meta.hosts.boschic = {
    admins = [ "seadoom" ];
    ipv4.address = "192.168.1.214";
    keys = {
      age = keys.age.boschic;
      ssh = [
        keys.ssh.boschic
        keys.ssh.boschic-rsa
      ];
    };
    network = "home";
    networks.ts = "100.112.94.38";
    users.seadoom.keys = {
      age = keys.age.seadoom-at-boschic;
      ssh = [ keys.ssh.seadoom-at-boschic ];
    };
    syncthing.id = "5TCUNJM-PVGGNJ6-DETAT3O-PSMTOEP-SXRT7FP-62EFNZY-6ENFIYZ-3J2VHQJ";
  };
}
